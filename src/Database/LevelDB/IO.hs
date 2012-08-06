{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Database.LevelDB.IO
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides a mid level interface to Google's
-- LevelDB (<http://leveldb.googlecode.com>).
-- 
-- It's based on the C API, and thus doesn't quite offer the entire
-- interface, but it gets close.
-- 
-- This binding is based on LevelDB v1.5.0.
-- 
-- Currently missing from the API:
-- 
--  * Support for @leveldb_writebatch_iterate@
-- 
--  * Iterator support
-- 
--  * Comparators
--
--  * Custom environment support (needs C library support)
--
--  * Custom filter policies besides bloom filters
-- 
-- TODO:
-- 
--  * Back Iterators/writebatches by 'ForeignPtr's with finalizers. Maybe snapshots too,
--    but normally you may want to control that more.
-- 
--  * More safety in 'close': it shouldn't double-free when you call it twice on the same object.
--    Perhaps make 'DB' instead contain an 'MVar DBState'
-- 
module Database.LevelDB.IO
       (
         -- * Types
         DB
       , Snapshot
       , Writebatch
       , Iterator
       , Compression(..)
       , DBOptions(..)
       , ReadOptions(..)
       , WriteOptions(..)
       , Range(..)
       , Err

         -- * Default options
       , defaultDBOptions
       , defaultReadOptions
       , defaultWriteOptions

         -- * Opening/closing databases
       , open              -- :: FilePath -> DBOptions -> IO (Either Err DB)
       , close             -- :: DB -> IO ()

         -- * Basic interface
       , put               -- :: DB -> WriteOptions -> ByteString -> ByteString -> IO (Maybe Err)
       , get               -- :: DB -> ReadOptions  -> ByteString -> IO (Either Err ByteString)
       , delete            -- :: DB -> WriteOptions -> ByteString -> IO (Maybe Err)

         -- * Batched writes
       , write             -- :: DB -> WriteOptions -> Writebatch -> IO (Maybe Err)
       , createWritebatch  -- :: IO Writebatch
       , clearWritebatch   -- :: Writebatch -> IO ()
       , writebatchPut     -- :: Writebatch -> ByteString -> ByteString -> IO ()
       , writebatchDelete  -- :: Writebatch -> ByteString -> IO ()
       , destroyWritebatch -- :: Writebatch -> IO ()

         -- * Comparators
         -- Custom comparators are not yet implemented

         -- * Filter policies
         -- Custom filter policies are not yet implemented

         -- * Iterators
         -- Iterators are not yet implemented

         -- * Snapshots
       , createSnapshot    -- :: DB -> IO Snapshot
       , releaseSnapshot   -- :: Snapshot -> IO DB

         -- * Destroying/Repairing databases
       , destroy           -- :: FilePath -> DBOptions -> IO (Maybe String)
       , repair            -- :: FilePath -> DBOptions -> IO (Maybe String)

         -- * Approximate sizes of filesystem data
       , approxSizes       -- :: DB -> [Range] -> IO [Word64]

         -- * Database compaction
       , compactRange      -- :: DB -> Maybe Range -> Maybe Range -> IO ()
       , compactAll        -- :: DB -> IO ()

         -- * Database properties
       , Property(..)      -- :: *
       , property          -- :: DB -> Property -> IO (Maybe String)

         -- * Re-exports
       , Default, def
       ) where

import Data.Word
import Control.Monad (liftM)
import Control.Applicative
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc

import Data.ByteString as S
import Data.ByteString.Unsafe as S

import Control.Concurrent as Conc

import Data.Default


import Database.LevelDB.FFI as C


-- 
-- Types
-- 

-- | 'DBState' encapsulates the state associated with a
-- database connection.
data DBState
  = DBState { _dbOptsPtr    :: !(Ptr LevelDB_Options_t)
            , _dbCachePtr   :: !(Maybe (Ptr LevelDB_Cache_t))
            , _dbFPolicyPtr :: !(Maybe (Ptr LevelDB_FilterPolicy_t))
            }
  deriving (Eq, Show)

-- | Handle to an opened database.
data DB
  = DB { _dbPtr   :: !(Ptr LevelDB_t)
       , _dbState :: !DBState
       }
  deriving (Eq, Show)

-- | Handle to a database snapshot
data Snapshot = Snapshot !(Ptr LevelDB_t) !(Ptr LevelDB_Snapshot_t)

-- | Handle to a batched write object
newtype Writebatch = Writebatch (Conc.MVar (Ptr LevelDB_Writebatch_t))
  
-- | Handle to a database iterator
newtype Iterator = Iterator (Conc.MVar (Ptr LevelDB_Iterator_t))

-- | Compression modes
data Compression
  = Snappy
  | None
  deriving (Eq, Show)

-- | Because of the way leveldb data is organized on disk, a single
-- 'get' call may involve multiple reads from disk. The optional
-- 'FilterPolicy' mechanism can be used to reduce the number of disk
-- reads substantially.
--
-- While custom filters can be set, right now only the included bloom
-- filter interface is available.
data FilterPolicy
  = Bloom {-# UNPACK #-} !Int -- ^ Bloom filter, with integer specifying how many bits per key to keep in memory.
  deriving (Eq, Show)

-- | Options for creating a database.
data DBOptions
  = DBOptions { -- dbComparator          :: ...
                dbCreateIfMissing     :: Bool         -- ^ Create database if it doesn't exist when calling 'open'. Default: 'False'.
              , dbErrorIfExists       :: Bool         -- ^ Error if the database exists when calling 'open'. Default: 'False'.
              , dbParanoidChecks      :: Bool         -- ^ Do aggressive checking of data integrity and report errors early. Default: 'False'

              , dbFilterPolicy        :: Maybe FilterPolicy -- ^ Set a filter policy for the database to reduce disk seeks.
                                                            -- Currently only a bloom filter can be set. Default: 'Nothing'

                --, dbInfoLog             :: ...
              , dbWriteBufferSize     :: Int          -- ^ Amount of data to buffer in memory before writing to disk. Default: 4MB.
              , dbMaxOpenFiles        :: Int          -- ^ Max amount of files allowed to be open at one time. Default: 1000.
              , dbCacheCapacity       :: Maybe Int    -- ^ Capacity of LRU cache. If 'Nothing', a default internal 8MB cache is used.
              , dbBlockSize           :: Int          -- ^ Size of on disk blocks, not taking compression into account.
                                                      -- Can change dynamically. Default: 4K.
              , dbBlockResizeInterval :: Int          -- ^ Number of keys between restart points. Most should leave this alone. Default: 16.
              , dbCompression         :: Compression  -- ^ Compression mode. Default: 'Snappy'.
              }
  deriving (Show)

-- | Default 'DBOptions'.
defaultDBOptions :: DBOptions
defaultDBOptions
  = DBOptions { -- dbComparator       = ...
                dbCreateIfMissing     = False
              , dbErrorIfExists       = False
              , dbParanoidChecks      = False
              , dbFilterPolicy        = Nothing
              --, dbInfoLog           = ...
              , dbWriteBufferSize     = 4194304
              , dbMaxOpenFiles        = 1000
              , dbCacheCapacity       = Nothing
              , dbBlockSize           = 4096
              , dbBlockResizeInterval = 16
              , dbCompression         = Snappy
              }

instance Default DBOptions where
  def = defaultDBOptions

-- | Options for issuing reads from the database.
data ReadOptions
  = ReadOptions { readVerifyChecksums :: Bool -- ^ If set, then reads will verify checksum data. Default: false.
                , readFillCache       :: Bool -- ^ Should data read be cached in memory? Default: true
                , readSnapshot        :: Maybe Snapshot -- ^ If set, reads will use this snapshot of the Database state.
                }

defaultReadOptions :: ReadOptions
defaultReadOptions
  = ReadOptions { readVerifyChecksums = False
                , readFillCache       = True
                , readSnapshot        = Nothing
                }

instance Default ReadOptions where
  def = defaultReadOptions

-- | Options specified when issuing writes on the database.
-- Right now, you can just control whether or not you should
-- sync the filesystem after every write via @fsync@ et al.
data WriteOptions
  = WriteOptions { writeWithSync :: Bool -- ^ Sync data after writes with @fsync@ et al. Default: False.
                 }

defaultWriteOptions :: WriteOptions
defaultWriteOptions
  = WriteOptions { writeWithSync = False
                 }

instance Default WriteOptions where
  def = defaultWriteOptions

-- | The LevelDB C API uses Strings for an error interface.
type Err = String

-- 
-- Interface 
-- 

-- | Open a database at a specified path.
-- May fail if database doesn't exist unless 'dbCreateIfMissing' is @True@.
-- Will fail if 'dbErrorIfExists' is set and the database exists.
open :: FilePath   -- ^ Path to database
     -> DBOptions  -- ^ Database options
     -> IO (Either Err DB)
open dbname dbopts
  = withCString dbname $ \str -> do
      st <- dbOptsToDBState dbopts
      r <- wrapErr (C.c_leveldb_open (_dbOptsPtr st) str)
      case r of
        Left e   -> return $ Left e
        Right db -> return $ Right $ DB db st

-- | Close a database handle.
close :: DB -- ^ Database
      -> IO ()
close (DB db st) = do
  C.c_leveldb_close db
  freeDBState st


-- | Put a value into a database.
put :: DB           -- ^ Database
    -> WriteOptions -- ^ Write options
    -> ByteString   -- ^ Key
    -> ByteString   -- ^ Value
    -> IO (Maybe Err)
put (DB db _) woptions key value
  = S.unsafeUseAsCStringLen key $ \(kptr,klen) -> do
      S.unsafeUseAsCStringLen value $ \(vptr,vlen) -> do
        opts <- writeOptsToPtr woptions
        r <- wrapErr (C.c_leveldb_put db opts kptr (fromIntegral klen) vptr (fromIntegral vlen))
        C.c_leveldb_writeoptions_destroy opts
        case r of
          Left e -> return $ Just e
          Right _ -> return Nothing

-- | Look up a value in a database. May return 'Right Data.ByteString.empty'
-- if the key does not exist.
get :: DB          -- ^ Database
    -> ReadOptions -- ^ Read options
    -> ByteString  -- ^ Key
    -> IO (Either Err ByteString)
get (DB db _) roptions key
  = S.unsafeUseAsCStringLen key $ \(kptr, klen) ->
      alloca $ \vallen -> do 
        opts <- readOptsToPtr roptions
        r    <- wrapErr (C.c_leveldb_get db opts kptr (fromIntegral klen) vallen)
        C.c_leveldb_readoptions_destroy opts
        case r of
          Left e  -> return $ Left e
          Right p -> do
            if p == nullPtr then return $ Right S.empty
             else do
              vlen <- peek vallen
              bs <- S.packCStringLen (p, fromIntegral vlen)
              return $ Right bs

-- | Remove a database entry. Returns 'Nothing' in case of success,
-- or 'Just e' where 'e' is an error if not.
delete :: DB           -- ^ Database
       -> WriteOptions -- ^ Write options
       -> ByteString   -- ^ Key
       -> IO (Maybe Err)
delete (DB db _) woptions key
  = S.unsafeUseAsCStringLen key $ \(kptr,klen) -> do
      opts <- writeOptsToPtr woptions
      r <- wrapErr (C.c_leveldb_delete db opts kptr (fromIntegral klen))
      C.c_leveldb_writeoptions_destroy opts
      case r of
        Left e  -> return $ Just e
        Right _ -> return Nothing


-- | Apply a 'WriteBatch' and all its updates to the database
-- atomically.
write :: DB           -- ^ Database
      -> WriteOptions -- ^ Write options
      -> Writebatch   -- ^ Batch of writes to issue
      -> IO (Maybe Err)
write (DB db _) woptions (Writebatch m) = do
  opts <- writeOptsToPtr woptions
  r <- Conc.withMVar m $ \wb -> wrapErr (C.c_leveldb_write db opts wb)
  C.c_leveldb_writeoptions_destroy opts
  case r of
    Left e  -> return $ Just e
    Right _ -> return Nothing

-- | Create a 'Writebatch', which can be used to make multiple atomic updates
createWritebatch :: IO Writebatch
createWritebatch
  = Writebatch <$> (C.c_leveldb_writebatch_create >>= Conc.newMVar)

-- | Clear all the update operations from the specified 'Writebatch' object
clearWritebatch :: Writebatch -> IO ()
clearWritebatch (Writebatch m)
  = Conc.withMVar m C.c_leveldb_writebatch_clear

-- | Issue a 'put' operation to take place as part of a write batch.
writebatchPut :: Writebatch -- ^ Write batch
              -> ByteString -- ^ Key
              -> ByteString -- ^ Value
              -> IO ()
writebatchPut (Writebatch m) key val
  = S.unsafeUseAsCStringLen key $ \(kptr, klen) ->
      S.unsafeUseAsCStringLen val $ \(vptr, vlen) -> 
        Conc.withMVar m $ \wb ->
          C.c_leveldb_writebatch_put wb kptr (fromIntegral klen) vptr (fromIntegral vlen)

-- | Issue a 'delete' operation to take place as part of a write batch.
writebatchDelete :: Writebatch -- ^ Write batch
                 -> ByteString -- ^ Key
                 -> IO ()
writebatchDelete (Writebatch m) key
  = S.unsafeUseAsCStringLen key $ \(kptr, klen) ->
      Conc.withMVar m $ \wb ->
        C.c_leveldb_writebatch_delete wb kptr (fromIntegral klen)

-- | Destroy a 'Writebatch' object after you're done with it.
destroyWritebatch :: Writebatch -- ^ Writebatch object
                  -> IO ()
destroyWritebatch (Writebatch m)
  = Conc.withMVar m C.c_leveldb_writebatch_destroy


-- | Return a handle to the current DB state. Iterators created with
-- this handle or reads issued with 'readSnapshot' set to this value will
-- observe a stable snapshot of the current DB state.  The caller must
-- call 'releaseSnapshot' when the snapshot is no longer needed.
createSnapshot :: DB -- ^ Database
               -> IO Snapshot
createSnapshot (DB db _) = do
  snap <- C.c_leveldb_create_snapshot db
  return $! Snapshot db snap

-- | Release a previously acquired snapshot.  The caller must not
-- use the 'Snapshot' again after this call.
releaseSnapshot :: Snapshot -- ^ Snapshot
                -> IO ()
releaseSnapshot (Snapshot db s) = do
  C.c_leveldb_release_snapshot db s


-- | Destroy the contents of the specified database.
-- Be very careful using this method.
-- 
-- Returns 'Nothing' if successful. Otherwise, returns
-- an error.
destroy :: FilePath  -- ^ Path to database
        -> DBOptions -- ^ Database options
        -> IO (Maybe String)
destroy dbname dbopts
  = withCString dbname $ \str -> do
      st <- dbOptsToDBState dbopts
      r <- wrapErr (C.c_leveldb_destroy_db (_dbOptsPtr st) str)
      freeDBState st
      case r of
        Left e -> return (Just e)
        Right _ -> return Nothing

-- | If a DB cannot be opened, you may attempt to call this method to
-- resurrect as much of the contents of the database as possible.
-- Some data may be lost, so be careful when calling this function
-- on a database that contains important information.
-- 
-- Returns 'Nothing' if there was no error. Otherwise, it returns
-- the error string.
repair :: FilePath  -- ^ Path to database
       -> DBOptions -- ^ Database options
       -> IO (Maybe Err)
repair dbname dbopts
  = withCString dbname $ \str -> do
      st <- dbOptsToDBState dbopts
      r <- wrapErr (C.c_leveldb_repair_db (_dbOptsPtr st) str)
      freeDBState st
      case r of
        Left e -> return (Just e)
        Right _ -> return Nothing


-- | A range represents a range of keys in the database.
data Range = Range {-# UNPACK #-} !ByteString
                   {-# UNPACK #-} !ByteString
  deriving (Eq, Show)

-- | Approximate the size of a range of values in the database.
approxSizes :: DB -> [Range] -> IO [Word64]
approxSizes _ _ = return []

-- | Compact a range of keys in the database. Deleted and overwritten
-- versions of old data are discarded and is rearranged to avoid
-- seeks/fragmentation.
--
-- The first parameter is the starting key, and the second is the
-- ending key.  'Nothing' represents a key before/after all the other
-- keys in the database.
--
-- Therefore, to compact the whole database, use @compactRange db
-- Nothing Nothing@ or 'compactAll' below.
compactRange :: DB -> Maybe Range -> Maybe Range -> IO ()
compactRange _ _ _ = return ()

-- | Compact the entire database. Defined as:
--
-- > compactAll db = compactRange db Nothing Nothing
compactAll :: DB -> IO ()
compactAll db = compactRange db Nothing Nothing


-- | Database properties. Currently offered properties are:
--
--  * \"leveldb.num-files-at-level\<N\>\" - return the number of files at level \<N\>,
--    where \<N\> is an ASCII representation of a level number (e.g. \"0\").
-- 
--  * \"leveldb.stats\" - returns a multi-line string that describes statistics
--    about the internal operation of the DB.
-- 
--  * \"leveldb.sstables\" - returns a multi-line string that describes all
--    of the sstables that make up the db contents.
data Property
  = NumFilesAtLevel {-# UNPACK #-} !Int
  | DBStats
  | SSTables
  deriving (Eq, Show)

-- | Retrieve a property about the database.
property :: DB 
         -> Property -- ^ Property
         -> IO (Maybe String)
property db (NumFilesAtLevel n)
  | n >= 0    = property' db $ "leveldb.num-files-at-level" ++ show n
  | otherwise = return Nothing
property db DBStats          = property' db "leveldb.stats"
property db SSTables         = property' db "leveldb.sstables"

property' :: DB -> String -> IO (Maybe String)
property' (DB db _) prop
  = withCString prop $ \str -> do
      p <- C.c_leveldb_property_value db str
      if p == nullPtr then return Nothing
       else do
         x <- Just `liftM` peekCString p
         free p
         return x

-- 
-- Utils
-- 

writeOptsToPtr :: WriteOptions -> IO (Ptr LevelDB_Writeoptions_t)
writeOptsToPtr WriteOptions{..} = do
  woptions <- C.c_leveldb_writeoptions_create
  C.c_leveldb_writeoptions_set_sync woptions (boolToNum writeWithSync)
  return woptions

readOptsToPtr :: ReadOptions -> IO (Ptr LevelDB_Readoptions_t)
readOptsToPtr ReadOptions{..} = do
  roptions <- C.c_leveldb_readoptions_create
  C.c_leveldb_readoptions_set_verify_checksums roptions (boolToNum readVerifyChecksums)
  C.c_leveldb_readoptions_set_fill_cache roptions (boolToNum readFillCache)
  case readSnapshot of
    Nothing             -> C.c_leveldb_readoptions_set_snapshot roptions nullPtr
    Just (Snapshot _ s) -> C.c_leveldb_readoptions_set_snapshot roptions s
  return roptions


freeDBState :: DBState -> IO ()
freeDBState DBState{..} = do
  C.c_leveldb_options_destroy _dbOptsPtr
  maybe (return ()) C.c_leveldb_cache_destroy _dbCachePtr
  maybe (return ()) C.c_leveldb_filterpolicy_destroy _dbFPolicyPtr

dbOptsToDBState :: DBOptions -> IO DBState
dbOptsToDBState dbopts@DBOptions{..} = do
  -- First set cache and policy options since they're
  -- delt with a little out of line.
  opts <- dbOptsToPtr dbopts
  cache <- case dbCacheCapacity of
    Just x -> do
      c <- C.c_leveldb_cache_create_lru (fromIntegral x)
      C.c_leveldb_options_set_cache opts c
      return $ Just c
    _ -> return Nothing
  fpolicy <- case dbFilterPolicy of
    Just (Bloom x) -> do
      fp <- C.c_leveldb_filterpolicy_create_bloom (fromIntegral x)
      C.c_leveldb_options_set_filter_policy opts fp
      return $ Just fp
    _ -> return Nothing
  -- Done
  return $! DBState opts cache fpolicy

-- NB: does not set cache!
dbOptsToPtr :: DBOptions -> IO (Ptr LevelDB_Options_t)
dbOptsToPtr DBOptions{..} = do
  options <- C.c_leveldb_options_create
  C.c_leveldb_options_set_create_if_missing options (boolToNum dbCreateIfMissing)
  C.c_leveldb_options_set_error_if_exists options (boolToNum dbErrorIfExists)
  C.c_leveldb_options_set_paranoid_checks options (boolToNum dbParanoidChecks)
  C.c_leveldb_options_set_write_buffer_size options (fromIntegral dbWriteBufferSize)
  C.c_leveldb_options_set_max_open_files options (fromIntegral dbMaxOpenFiles)
  C.c_leveldb_options_set_block_size options (fromIntegral dbBlockSize)
  C.c_leveldb_options_set_block_restart_interval options (fromIntegral dbBlockResizeInterval)
  C.c_leveldb_options_set_compression options (toCompression dbCompression)

  return options
 where
    toCompression Snappy = C.c_leveldb_snappy_compression
    toCompression None   = C.c_leveldb_no_compression

boolToNum :: Num b => Bool -> b
boolToNum True = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)

wrapErr :: (Ptr CString -> IO a) -> IO (Either Err a)
wrapErr f = do
  alloca $ \ptr -> do
    poke ptr nullPtr
    r <- f ptr
    x <- peek ptr
    if (x /= nullPtr) then do
      str <- peekCString x
      free x
      return $ Left str
     else do
      return $ Right r
