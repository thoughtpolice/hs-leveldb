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
-- This binding is based on LevelDB git revision \'239ac9d2dea0ac1708b7d903a3d80d3883e0781b\'
-- 
module Database.LevelDB
       (
         -- * Types
         Snapshot
       , Compression(..)
       , DBOptions(..)
       , ReadOptions(..)
       , WriteOptions(..)

         -- * Opening/closing databases

         -- * Basic interface

         -- * Batched writes

         -- * Iterators

         -- * Snapshots

         -- * Destroying/Repairing databases

         -- * Approximate sizes of filesystem data

         -- * Database properties

         -- * FFI interface
       ) where

#include <leveldb/c.h>

data Snapshot
  
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


-- 
-- FFI
-- 


