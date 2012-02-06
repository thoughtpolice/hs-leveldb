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

         -- * Opening/closing databases
       , open              -- :: DBOptions -> String -> IO (Either Err DB)
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
       , releaseSnapshot   -- :: DB -> IO Snapshot

         -- * Destroying/Repairing databases
       , destroy           -- :: DBOptions -> String -> IO (Maybe String)
       , repair            -- :: DBOptions -> String -> IO (Maybe String)

         -- * Approximate sizes of filesystem data

         -- * Database properties
       , property          -- :: DB -> String -> IO String
       ) where
import Foreign.Ptr
import Data.ByteString as S

import Database.LevelDB.FFI


-- Types

data DB = DB !(Ptr LevelDB_t)

data Snapshot = Snapshot !(Ptr LevelDB_Snapshot_t)

data Writebatch = Writebatch !(Ptr LevelDB_Writebatch_t)
  
data Iterator = Iterator !(Ptr LevelDB_Iterator_t)

data Compression
  = Snappy
  | None
  deriving (Eq, Show)

-- | Options for creating a database.
data DBOptions
  = DBOptions { -- dbComparator          :: ...
                dbCreateIfMissing     :: Bool
              , dbErrorIfExists       :: Bool
              , dbParanoidChecks      :: Bool
              --, dbInfoLog             :: Bool
              , dbWriteBufferSize     :: Int
              , dbMaxOpenFiles        :: Int
              , dbCacheCapacity       :: Int
              , dbBlockSize           :: Int
              , dbBlockResizeInterval :: Int
              , dbCompression         :: Compression
              }
  deriving (Show)

-- | Options for issuing reads from the database.
data ReadOptions
  = ReadOptions { readVerifyChecksums :: Bool
                , readFillCache       :: Bool
                , readSnapshot        :: Snapshot
                }

-- | Options specified when issuing writes on the database.
-- Right now, you can just control whether or not you should
-- sync the filesystem after every write.
data WriteOptions
  = WriteOptions { writeWithSync :: Bool
                 }

type Err = String

-- Interface 

open :: DBOptions -> String -> IO (Either Err DB)
open = undefined

close :: DB -> IO ()
close = undefined

put :: DB -> WriteOptions -> ByteString -> ByteString -> IO (Maybe Err)
put = undefined

get :: DB -> ReadOptions  -> ByteString -> IO (Either Err ByteString)
get = undefined

delete :: DB -> WriteOptions -> ByteString -> IO (Maybe Err)
delete = undefined

write :: DB -> WriteOptions -> Writebatch -> IO (Maybe Err)
write = undefined

createWritebatch :: IO Writebatch
createWritebatch = undefined

writebatchPut :: Writebatch -> ByteString -> ByteString -> IO ()
writebatchPut = undefined

writebatchDelete :: Writebatch -> ByteString -> IO ()
writebatchDelete = undefined

destroyWritebatch :: Writebatch -> IO ()
destroyWritebatch = undefined

createSnapshot :: DB -> IO Snapshot
createSnapshot = undefined

releaseSnapshot :: DB -> IO Snapshot
releaseSnapshot = undefined

destroy :: DBOptions -> String -> IO (Maybe String)
destroy = undefined

repair :: DBOptions -> String -> IO (Maybe String)
repair = undefined

property :: DB -> String -> IO String
property = undefined
