-- |
-- Module      : Database.LevelDB
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides a moderately high level interface to Google's
-- LevelDB (<http://leveldb.googlecode.com>).
-- 
-- It's based on the C API, and thus doesn't quite offer the entire
-- interface, but it gets close.
-- 
-- This binding is based on LevelDB git revision \'239ac9d2dea0ac1708b7d903a3d80d3883e0781b\'.
-- 
module Database.LevelDB
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
       , Err
       
         -- * Default options
       , defaultDBOptions
       , defaultReadOptions
       , defaultWriteOptions

         -- * Opening/closing databases
       , open              -- :: DBOptions -> FilePath -> IO (Either Err DB)
       , close             -- :: DB -> IO ()

         -- * Basic interface
       , put               -- :: DB -> WriteOptions -> ByteString -> ByteString -> IO (Maybe Err)
       , get               -- :: DB -> ReadOptions  -> ByteString -> IO (Either Err ByteString)
       , delete            -- :: DB -> WriteOptions -> ByteString -> IO (Maybe Err)

         -- * Batched writes
       , write             -- :: DB -> WriteOptions -> Writebatch -> IO (Maybe Err)
       , createWritebatch  -- :: IO Writebatch
       , writebatchPut     -- :: Writebatch -> ByteString -> ByteString -> IO ()
       , writebatchDelete  -- :: Writebatch -> ByteString -> IO ()
       , destroyWritebatch -- :: Writebatch -> IO ()

         -- * Iterators

         -- * Snapshots
       , createSnapshot    -- :: DB -> IO Snapshot
       , releaseSnapshot   -- :: Snapshot -> IO DB

         -- * Destroying/Repairing databases
       , destroy           -- :: DBOptions -> FilePath -> IO (Maybe String)
       , repair            -- :: DBOptions -> FilePath -> IO (Maybe String)

         -- * Approximate sizes of filesystem data

         -- * Database properties
       , property          -- :: DB -> String -> IO (Maybe String)
       ) where
import Foreign.Ptr
import System.FilePath
import Data.ByteString as S

import Database.LevelDB.FFI


-- 
-- Types
-- 

-- | Handle to an opened database.
data DB = DB !(Ptr LevelDB_t)

-- | Handle to a database snapshot
data Snapshot = Snapshot !(Ptr LevelDB_Snapshot_t)

-- | Handle to a batched write object
data Writebatch = Writebatch !(Ptr LevelDB_Writebatch_t)
  
-- | Handle to a database iterator
data Iterator = Iterator !(Ptr LevelDB_Iterator_t)

-- | Compression modes
data Compression
  = Snappy
  | None
  deriving (Eq, Show)

-- | Options for creating a database.
data DBOptions
  = DBOptions { -- dbComparator          :: ...
                dbCreateIfMissing     :: Bool        -- ^ Create database if it doesn't exist when calling 'open'. Default: False.
              , dbErrorIfExists       :: Bool        -- ^ Error if the database exists when calling 'open': Default: False.
              , dbParanoidChecks      :: Bool        -- ^ Do aggressive checking of data integrity and report errors early. Default: False

              --, dbInfoLog             :: ...
              , dbWriteBufferSize     :: Int         -- ^ Amount of data to buffer in memory before writing to disk. Default: 4MB.
              , dbMaxOpenFiles        :: Int         -- ^ Max amount of files allowed to be open at one time. Default: 1000.
              , dbCacheCapacity       :: Maybe Int   -- ^ Capacity of LRU cache. If 'Nothing', a default internal 8MB cache is used.
              , dbBlockSize           :: Int         -- ^ Size of on disk blocks, not taking compression into account.
                                                     -- Can change dynamically. Default: 4K.
              , dbBlockResizeInterval :: Int         -- ^ Number of keys between restart points. Most should leave this alone. Default: 16.
              , dbCompression         :: Compression -- ^ Compression mode. Default: 'Snappy'.
              }
  deriving (Show)

-- | Default 'DBOptions'.
defaultDBOptions :: DBOptions
defaultDBOptions
  = DBOptions { -- dbComparator       = ...
                dbCreateIfMissing     = False
              , dbErrorIfExists       = False
              , dbParanoidChecks      = False
              --, dbInfoLog           = ...
              , dbWriteBufferSize     = 4194304
              , dbMaxOpenFiles        = 1000
              , dbCacheCapacity       = Nothing
              , dbBlockSize           = 4096
              , dbBlockResizeInterval = 16
              , dbCompression         = Snappy
              }

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

-- | The LevelDB C API uses Strings for an error interface.
type Err = String

-- 
-- Interface 
-- 

-- | Open a database at a specified path.
-- May fail if database doesn't exist unless 'dbCreateIfMissing' is @True@.
-- Will fail if 'dbErrorIfExists' is set and the database exists.
open :: DBOptions  -- ^ Database options
     -> FilePath   -- ^ Path to database
     -> IO (Either Err DB)
open = undefined

-- | Close a database handle.
close :: DB -- ^ Database
      -> IO ()
close = undefined

-- | Put a value into a database.
put :: DB           -- ^ Database
    -> WriteOptions -- ^ Write options
    -> ByteString   -- ^ Key
    -> ByteString   -- ^ Value
    -> IO (Maybe Err)
put = undefined

get :: DB          -- ^ Database
    -> ReadOptions -- ^ Read options
    -> ByteString  -- ^ Key
    -> IO (Either Err ByteString)
get = undefined

-- | Remove a database entry
delete :: DB           -- ^ Database
       -> WriteOptions -- ^ Write options
       -> ByteString   -- ^ Key
       -> IO (Maybe Err)
delete = undefined

-- | Apply a 'WriteBatch' and all its updates to the database
-- atomically.
write :: DB           -- ^ Database
      -> WriteOptions -- ^ Write options
      -> Writebatch   -- ^ Batch of writes to issue
      -> IO (Maybe Err)
write = undefined

-- | Create a 'Writebatch', which can be used to make multiple atomic updates
createWritebatch :: IO Writebatch
createWritebatch = undefined

-- | Issue a 'put' operation to take place as part of a write batch.
writebatchPut :: Writebatch -- ^ Write batch
              -> ByteString -- ^ Key
              -> ByteString -- ^ Value
              -> IO ()
writebatchPut = undefined

-- | Issue a 'delete' operation to take place as part of a write batch.
writebatchDelete :: Writebatch -- ^ Write batch
                 -> ByteString -- ^ Key
                 -> IO ()
writebatchDelete = undefined

-- | Destroy a 'Writebatch' object after you're done with it
destroyWritebatch :: Writebatch -- ^ Writebatch object
                  -> IO ()
destroyWritebatch = undefined

-- | Return a handle to the current DB state. Iterators created with
-- this handle or reads issued with 'readSnapshot' set to this value will
-- observe a stable snapshot of the current DB state.  The caller must
-- call 'releaseSnapshot' when the snapshot is no longer needed.
createSnapshot :: DB -- ^ Database
               -> IO Snapshot
createSnapshot = undefined

-- | Release a previously acquired snapshot.  The caller must not
-- use "snapshot" after this call.
releaseSnapshot :: Snapshot -- ^ Snapshot
                -> IO ()
releaseSnapshot = undefined

-- | Destroy the contents of the specified database.
-- Be very careful using this method.
destroy :: DBOptions -- ^ Database options
        -> FilePath  -- ^ Path to database
        -> IO (Maybe String)
destroy = undefined

-- | If a DB cannot be opened, you may attempt to call this method to
-- resurrect as much of the contents of the database as possible.
-- Some data may be lost, so be careful when calling this function
-- on a database that contains important information.
repair :: DBOptions -- ^ Database options
       -> FilePath  -- ^ Path to database
       -> IO (Maybe String)
repair = undefined

-- | Retrieve a property about the database. Valid property names include:
--
--  * \"leveldb.num-files-at-level\<N\>\" - return the number of files at level \<N\>,
--    where <N> is an ASCII representation of a level number (e.g. \"0\").
-- 
--  * \"leveldb.stats\" - returns a multi-line string that describes statistics
--    about the internal operation of the DB.
-- 
--  * \"leveldb.sstables\" - returns a multi-line string that describes all
--    of the sstables that make up the db contents.
property :: DB 
         -> String -- ^ Property name
         -> IO (Either Err String)
property = undefined
