-- |
-- Module      : Database.LevelDB
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides a high level interface to Google's
-- LevelDB (<http://leveldb.googlecode.com>). It is based
-- on the 'ResourceT' monad transformer and the 'cereal'
-- serialization interface.
-- 
-- This binding is based on LevelDB v1.5.0.
-- 
module Database.LevelDB
       ( -- * Types
         DB                     -- :: *
       , DBOptions              -- :: *
       , WriteOptions           -- :: *
       , ReadOptions            -- :: *
       , Range(..)              -- :: *

         -- * Opening and closing a database
       , open                   -- :: MonadResource m => ...

         -- * Basic interface
       , put                    -- :: MonadResource m => ...
       , putBS                  -- :: MonadResource m => ...
       , get                    -- :: MonadResource m => ...
       , getBS                  -- :: MonadResource m => ...
       , delete                 -- :: MonadResource m => ...
       , deleteBS               -- :: MonadResource m => ...

         -- * Batched writes
         -- TODO

         -- * Comparators
         -- TODO

         -- * Filter policies
         -- TODO

         -- * Iterators
         -- TODO

         -- * Taking snapshots
       , snapshot               -- :: MonadResource m => LevelDB.DB -> m (ReleaseKey, LevelDB.Snapshot)
       , withSnapshot           -- :: MonadResource m => DB -> (Snapshot -> m a) -> m a

         -- * Repairing and destroying databases
       , LevelDB.repair         -- :: FilePath -> DBOptions -> IO (Maybe String)
       , LevelDB.destroy        -- :: FilePath -> DBOptions -> IO (Maybe String)

         -- * Approximate sizes of filesystem data
       , approxSizes            -- :: MonadResource m => DB -> [Range] -> m [Word64]

         -- * Database compaction
       , compactRange           -- :: MonadResource m => DB -> Maybe Range -> Maybe Range -> m ()
       , compactAll             -- :: MonadResource m => DB -> m ()

         -- * Database properties
       , Property(..)           -- :: *
       , property               -- :: MonadResource m => DB -> Property -> m (Maybe String)

         -- * Re-exports
       , Default, def
       ) where

import Data.Word
import Data.Default (Default, def)
import Control.Applicative

import Data.ByteString
import Data.Serialize hiding (get, put)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource

import Database.LevelDB.IO (Range(..), Property(..),
                            DB, DBOptions, WriteOptions, ReadOptions,
                            Snapshot)
import qualified Database.LevelDB.IO as LevelDB

-- | Opens a database. The handle is automatically closed when the enclosing
-- 'runResourceT' has ended.
open :: MonadResource m => FilePath -> DBOptions -> m DB
open path opts = snd <$> allocate (open' path opts) LevelDB.close

open' :: FilePath -> DBOptions -> IO DB
open' path opts
  =   LevelDB.open path opts
  >>= onoesIfErr ("LevelDB: Could not open database " ++ path)

-- | Stick a key/value pair in the database; the key and value can be
-- any instance of 'Serialize'.
--
-- Defined as:
--
-- > put db wopts a b = putBS db wopts (encode a) (encode b)
put :: (MonadResource m, Serialize a, Serialize b) => DB -> WriteOptions -> a -> b -> m ()
put db wopts a b = putBS db wopts (encode a) (encode b)

-- | Stick a key/value pair in the database.
putBS :: MonadResource m => DB -> LevelDB.WriteOptions -> ByteString -> ByteString -> m ()
putBS db wopts k v
  =   liftIO (LevelDB.put db wopts k v)
  >>= onoesIfErr2 "LevelDB: Could not put value in database"

-- | Pull a value out of the database.
--
-- Defined as:
--
-- > get db ropts k = decodeWithErrors <$> getBS db ropts (encode k)
get :: (MonadResource m, Serialize a, Serialize b) => DB -> ReadOptions -> a -> m b
get db opts k = do
  r <- decode <$> getBS db opts (encode k)
  onoesIfErr "LevelDB: Serialize decoding error" r

-- | Pull a value out of the database.
getBS :: MonadResource m => DB -> ReadOptions -> ByteString -> m ByteString
getBS db opts k
  =   liftIO (LevelDB.get db opts k)
  >>= onoesIfErr "LevelDB: Could not get value"

-- | Delete a value from the database.
--
-- Defined as:
--
-- > delete db wopts k = deleteBS db wopts (encode k)
delete :: (MonadResource m, Serialize a) => DB -> WriteOptions -> a -> m ()
delete db wopts k = deleteBS db wopts (encode k)

-- | Delete a value from the database.
deleteBS :: MonadResource m => DB -> WriteOptions -> ByteString -> m ()
deleteBS db wopts k
  =   liftIO (LevelDB.delete db wopts k)
  >>= onoesIfErr2 "LevelDB: Could not delete value"

-- | Takes a snapshot of a database. You can use the returned 'ReleaseKey'
-- to free the snapshot manually, or let it get disposed of automatically when
-- the enclosing 'runResourceT' ends.
snapshot :: MonadResource m => LevelDB.DB -> m (ReleaseKey, LevelDB.Snapshot)
snapshot db = allocate (LevelDB.createSnapshot db) LevelDB.releaseSnapshot

-- | Run an action with a snapshot of the database. The snapshot will be
-- released after the action has been run.
withSnapshot :: MonadResource m => DB -> (Snapshot -> m a) -> m a
withSnapshot db f = do
  (key, snap) <- snapshot db
  r <- f snap
  release key
  return r

-- | Approximate the size of a range of values in the database.
approxSizes :: MonadResource m => LevelDB.DB -> [Range] -> m [Word64]
approxSizes db ranges = liftIO (LevelDB.approxSizes db ranges)

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
compactRange :: MonadResource m => LevelDB.DB -> Maybe Range -> Maybe Range -> m ()
compactRange db r1 r2 = liftIO (LevelDB.compactRange db r1 r2)

-- | Compact all the keys/values in the entire database.
compactAll :: MonadResource m => LevelDB.DB -> m ()
compactAll db = liftIO (LevelDB.compactAll db)

-- | Retrieve a 'LevelDB.Property' about the database.
property :: MonadResource m => LevelDB.DB -> Property -> m (Maybe String)
property db prop = liftIO (LevelDB.property db prop)


--
-- Misc helpers
--

onoesIfErr :: (MonadThrow m, Show e) => String -> Either e a -> m a
onoesIfErr msg (Left e)  = throwUserErr (msg ++ ": "++show e)
onoesIfErr _   (Right r) = return r

onoesIfErr2 :: (MonadThrow m, Show e) => String -> Maybe e -> m ()
onoesIfErr2 msg (Just e) = throwUserErr (msg ++ ": "++show e)
onoesIfErr2 _   Nothing  = return ()

throwUserErr :: MonadThrow m => String -> m a
throwUserErr = monadThrow . userError
