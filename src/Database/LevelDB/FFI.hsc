-- |
-- Module      : Database.LevelDB.FFI
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides low-level FFI bindings to the 'LevelDB' C API.
-- 
-- This binding is based on LevelDB git revision \'239ac9d2dea0ac1708b7d903a3d80d3883e0781b\'.
-- 
module Database.LevelDB.FFI
       ( -- * Types
         LevelDB_t
       , LevelDB_Cache_t
       , LevelDB_Comparator_t
       , LevelDB_Env_t
       , LevelDB_Iterator_t
       , LevelDB_Options_t
       , LevelDB_Readoptions_t
       , LevelDB_Writeoptions_t
       , LevelDB_Snapshot_t
       , LevelDB_Writebatch_t

         -- * Bindings
       , c_leveldb_open
       , c_leveldb_close
       , c_leveldb_destroy_db
       , c_leveldb_repair_db

       , c_leveldb_put
       , c_leveldb_delete
       , c_leveldb_get
       , c_leveldb_write

{-
       , c_leveldb_create_default_env
       , c_leveldb_cache_create_lru
       , c_leveldb_options_create
       , c_leveldb_readoptions_create
       , c_leveldb_writeoptions_create
-}
       ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <leveldb/c.h>

-- Phantom types
data LevelDB_t
data LevelDB_Cache_t
data LevelDB_Comparator_t
data LevelDB_Env_t
data LevelDB_Iterator_t
data LevelDB_Options_t
data LevelDB_Readoptions_t
data LevelDB_Writeoptions_t
data LevelDB_Snapshot_t
data LevelDB_Writebatch_t

-- Raw FFI bindings

foreign import ccall "leveldb_open"
  c_leveldb_open :: Ptr LevelDB_Options_t -> CString -> Ptr CString -> IO (Ptr LevelDB_t)

foreign import ccall "leveldb_close"
  c_leveldb_close :: Ptr LevelDB_t -> IO ()

foreign import ccall "leveldb_destroy_db"
  c_leveldb_destroy_db :: Ptr LevelDB_Options_t -> CString -> Ptr CString -> IO ()

foreign import ccall "leveldb_repair_db"
  c_leveldb_repair_db :: Ptr LevelDB_Options_t -> CString -> Ptr CString -> IO ()


foreign import ccall "leveldb_put"
  c_leveldb_put :: Ptr LevelDB_t -> Ptr LevelDB_Writeoptions_t 
                -> CString -> CSize -> CString -> CSize
                -> Ptr CString -> IO ()

foreign import ccall "leveldb_delete"
  c_leveldb_delete :: Ptr LevelDB_t -> Ptr LevelDB_Writeoptions_t
                   -> CString -> CSize
                   -> Ptr CString -> IO ()

foreign import ccall "leveldb_get"
  c_leveldb_get :: Ptr LevelDB_t -> Ptr LevelDB_Readoptions_t
                -> CString -> CSize -> Ptr CSize
                -> Ptr CString -> IO CString

foreign import ccall "leveldb_write"
  c_leveldb_write :: Ptr LevelDB_t -> Ptr LevelDB_Writeoptions_t
                  -> Ptr LevelDB_Writebatch_t
                  -> Ptr CString -> IO ()

