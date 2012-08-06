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
         LevelDB.DB             -- :: *
       , Range(..)              -- :: *

         -- * Opening and closing a database
       , open                   -- :: MonadResource m => ...

         -- * Basic interface
       , put                    -- :: MonadResource m => ...
       , get                    -- :: MonadResource m => ...
       , delete                 -- :: MonadResource m => ...

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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource

import Database.LevelDB.IO (Range(..), Property(..))
import qualified Database.LevelDB.IO as LevelDB

-- | Opens a database. The handle is automatically closed when the enclosing
-- 'runResourceT' has ended.
open :: MonadResource m => FilePath -> LevelDB.DBOptions -> m LevelDB.DB
open path opts = snd <$> allocate (open' path opts) LevelDB.close

open' :: FilePath -> LevelDB.DBOptions -> IO LevelDB.DB
open' path opts = do
  r <- LevelDB.open path opts
  case r of
    Left err ->
      let e = userError ("Could not open database " ++ path ++ ", " ++ err)
      in monadThrow e
    Right db -> return db

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
