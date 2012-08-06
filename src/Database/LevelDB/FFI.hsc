{-# LANGUAGE CPP #-}
-- |
-- Module      : Database.LevelDB.FFI
-- Copyright   : (c) Austin Seipp 2012
-- License     : BSD3
-- 
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides low-level FFI bindings to the LevelDB C API.
-- 
-- This binding is based on LevelDB v1.5.0.
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
       , LevelDB_FilterPolicy_t

         -- * Bindings
         -- ** Open, closing, destroying, repairing
       , c_leveldb_open
       , c_leveldb_close
       , c_leveldb_destroy_db
       , c_leveldb_repair_db

         -- ** Basic interface
       , c_leveldb_put
       , c_leveldb_delete
       , c_leveldb_get
       , c_leveldb_write

         -- ** Object creation/destruction
       , c_leveldb_create_default_env
       , c_leveldb_env_destroy
       , c_leveldb_cache_create_lru
       , c_leveldb_cache_destroy
       , c_leveldb_filterpolicy_create_bloom
       , c_leveldb_filterpolicy_destroy
       , c_leveldb_options_create
       , c_leveldb_options_destroy
       , c_leveldb_readoptions_create
       , c_leveldb_readoptions_destroy
       , c_leveldb_writeoptions_create
       , c_leveldb_writeoptions_destroy
       , c_leveldb_writebatch_create
       , c_leveldb_writebatch_destroy
       , c_leveldb_create_iterator
       , c_leveldb_iter_destroy
       , c_leveldb_create_snapshot
       , c_leveldb_release_snapshot
       --, c_leveldb_comparator_create
       , c_leveldb_comparator_destroy

         -- ** Regular options
       , c_leveldb_options_set_create_if_missing
       , c_leveldb_options_set_error_if_exists
       , c_leveldb_options_set_paranoid_checks
       , c_leveldb_options_set_cache
       , c_leveldb_options_set_env
       , c_leveldb_options_set_write_buffer_size
       , c_leveldb_options_set_max_open_files
       , c_leveldb_options_set_block_size
       , c_leveldb_options_set_block_restart_interval
       , c_leveldb_options_set_compression
       , c_leveldb_options_set_comparator
       , c_leveldb_options_set_filter_policy
       , c_leveldb_no_compression
       , c_leveldb_snappy_compression

         -- ** Read options
       , c_leveldb_readoptions_set_verify_checksums
       , c_leveldb_readoptions_set_fill_cache
       , c_leveldb_readoptions_set_snapshot

         -- ** Write options
       , c_leveldb_writeoptions_set_sync

         -- ** Writebatches
       , c_leveldb_writebatch_clear
       , c_leveldb_writebatch_put
       , c_leveldb_writebatch_delete
       -- , c_leveldb_writebatch_iterate

         -- ** Iterators
       , c_leveldb_iter_valid
       , c_leveldb_iter_seek_to_first
       , c_leveldb_iter_seek_to_last
       , c_leveldb_iter_seek
       , c_leveldb_iter_next
       , c_leveldb_iter_prev
       , c_leveldb_iter_key
       , c_leveldb_iter_value
       , c_leveldb_iter_get_error

         -- ** Approximate sizes
       , c_leveldb_approximate_sizes

         -- ** Compacting
       , c_leveldb_compact_range

         -- ** Properties
       , c_leveldb_property_value
       ) where

import Data.Word

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
data LevelDB_FilterPolicy_t

-- 
-- Raw FFI bindings
-- 

-- Opening/closing/destroying/repairing

foreign import ccall "leveldb_open"
  c_leveldb_open :: Ptr LevelDB_Options_t -> CString -> Ptr CString -> IO (Ptr LevelDB_t)

foreign import ccall "leveldb_close"
  c_leveldb_close :: Ptr LevelDB_t -> IO ()

foreign import ccall "leveldb_destroy_db"
  c_leveldb_destroy_db :: Ptr LevelDB_Options_t -> CString -> Ptr CString -> IO ()

foreign import ccall "leveldb_repair_db"
  c_leveldb_repair_db :: Ptr LevelDB_Options_t -> CString -> Ptr CString -> IO ()

-- Basic interface

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

-- Creating/destroying objects

foreign import ccall "leveldb_create_default_env"
  c_leveldb_create_default_env :: IO (Ptr LevelDB_Env_t)
foreign import ccall "leveldb_env_destroy"
  c_leveldb_env_destroy :: Ptr LevelDB_Env_t -> IO ()

foreign import ccall "leveldb_cache_create_lru"
  c_leveldb_cache_create_lru :: CSize -> IO (Ptr LevelDB_Cache_t)
foreign import ccall "leveldb_cache_destroy"
  c_leveldb_cache_destroy :: Ptr LevelDB_Cache_t -> IO ()

foreign import ccall "leveldb_filterpolicy_create_bloom"
  c_leveldb_filterpolicy_create_bloom :: CInt -> IO (Ptr LevelDB_FilterPolicy_t)
foreign import ccall "leveldb_filterpolicy_destroy"
  c_leveldb_filterpolicy_destroy :: Ptr LevelDB_FilterPolicy_t -> IO ()

foreign import ccall "leveldb_options_create"
  c_leveldb_options_create :: IO (Ptr LevelDB_Options_t)
foreign import ccall "leveldb_options_destroy"
  c_leveldb_options_destroy :: Ptr LevelDB_Options_t -> IO ()

foreign import ccall "leveldb_readoptions_create"
  c_leveldb_readoptions_create :: IO (Ptr LevelDB_Readoptions_t)
foreign import ccall "leveldb_readoptions_destroy"
  c_leveldb_readoptions_destroy :: Ptr LevelDB_Readoptions_t -> IO ()

foreign import ccall "leveldb_writeoptions_create"
  c_leveldb_writeoptions_create :: IO (Ptr LevelDB_Writeoptions_t)
foreign import ccall "leveldb_writeoptions_destroy"
  c_leveldb_writeoptions_destroy :: Ptr LevelDB_Writeoptions_t -> IO ()

foreign import ccall "leveldb_writebatch_create"
  c_leveldb_writebatch_create :: IO (Ptr LevelDB_Writebatch_t)
foreign import ccall "leveldb_writebatch_destroy"
  c_leveldb_writebatch_destroy :: Ptr LevelDB_Writebatch_t -> IO ()

foreign import ccall "leveldb_create_iterator"
  c_leveldb_create_iterator :: Ptr LevelDB_t -> Ptr LevelDB_Readoptions_t -> IO (Ptr LevelDB_Iterator_t)
foreign import ccall "leveldb_iter_destroy"
  c_leveldb_iter_destroy :: Ptr LevelDB_Iterator_t -> IO ()

foreign import ccall "leveldb_create_snapshot"
  c_leveldb_create_snapshot :: Ptr LevelDB_t -> IO (Ptr LevelDB_Snapshot_t)
foreign import ccall "leveldb_release_snapshot"
  c_leveldb_release_snapshot :: Ptr LevelDB_t -> Ptr LevelDB_Snapshot_t -> IO ()


{-
foreign import ccall "leveldb_comparator_create"
  c_leveldb_comparator_create :: 
-}
foreign import ccall "leveldb_comparator_destroy"
  c_leveldb_comparator_destroy :: Ptr LevelDB_Comparator_t -> IO ()

-- LevelDB options

foreign import ccall "leveldb_options_set_create_if_missing"
  c_leveldb_options_set_create_if_missing :: Ptr LevelDB_Options_t -> CUChar -> IO ()

foreign import ccall "leveldb_options_set_error_if_exists"
  c_leveldb_options_set_error_if_exists :: Ptr LevelDB_Options_t -> CUChar -> IO ()

foreign import ccall "leveldb_options_set_paranoid_checks"
  c_leveldb_options_set_paranoid_checks :: Ptr LevelDB_Options_t -> CUChar -> IO ()

foreign import ccall "leveldb_options_set_cache"
  c_leveldb_options_set_cache :: Ptr LevelDB_Options_t -> Ptr LevelDB_Cache_t -> IO ()

foreign import ccall "leveldb_options_set_env"
  c_leveldb_options_set_env :: Ptr LevelDB_Options_t -> Ptr LevelDB_Env_t -> IO ()

foreign import ccall "leveldb_options_set_write_buffer_size"
  c_leveldb_options_set_write_buffer_size :: Ptr LevelDB_Options_t -> CSize -> IO ()

foreign import ccall "leveldb_options_set_max_open_files"
  c_leveldb_options_set_max_open_files :: Ptr LevelDB_Options_t -> CInt -> IO ()

foreign import ccall "leveldb_options_set_block_size"
  c_leveldb_options_set_block_size :: Ptr LevelDB_Options_t -> CSize -> IO ()

foreign import ccall "leveldb_options_set_block_restart_interval"
  c_leveldb_options_set_block_restart_interval :: Ptr LevelDB_Options_t -> CInt -> IO ()

foreign import ccall "leveldb_options_set_compression"
  c_leveldb_options_set_compression :: Ptr LevelDB_Options_t -> CInt -> IO ()

foreign import ccall "leveldb_options_set_comparator"
  c_leveldb_options_set_comparator :: Ptr LevelDB_Options_t -> Ptr LevelDB_Comparator_t -> IO ()

foreign import ccall "leveldb_options_set_filter_policy"
  c_leveldb_options_set_filter_policy :: Ptr LevelDB_Options_t -> Ptr LevelDB_FilterPolicy_t -> IO ()

c_leveldb_no_compression :: CInt
c_leveldb_no_compression = fromIntegral (0 :: Int)

c_leveldb_snappy_compression :: CInt
c_leveldb_snappy_compression = fromIntegral (1 :: Int)

-- Readoptions

foreign import ccall "leveldb_readoptions_set_verify_checksums"
  c_leveldb_readoptions_set_verify_checksums :: Ptr LevelDB_Readoptions_t -> CUChar -> IO ()

foreign import ccall "leveldb_readoptions_set_fill_cache"
  c_leveldb_readoptions_set_fill_cache :: Ptr LevelDB_Readoptions_t -> CUChar -> IO ()

foreign import ccall "leveldb_readoptions_set_snapshot"
  c_leveldb_readoptions_set_snapshot :: Ptr LevelDB_Readoptions_t -> Ptr LevelDB_Snapshot_t -> IO ()

-- Writeoptions

foreign import ccall "leveldb_writeoptions_set_sync"
  c_leveldb_writeoptions_set_sync :: Ptr LevelDB_Writeoptions_t -> CUChar -> IO ()

-- Writebatches

foreign import ccall "leveldb_writebatch_clear"
  c_leveldb_writebatch_clear :: Ptr LevelDB_Writebatch_t -> IO ()

foreign import ccall "leveldb_writebatch_put"
  c_leveldb_writebatch_put :: Ptr LevelDB_Writebatch_t
                           -> CString -> CSize
                           -> CString -> CSize
                           -> IO ()

foreign import ccall "leveldb_writebatch_delete"
  c_leveldb_writebatch_delete :: Ptr LevelDB_Writebatch_t
                              -> CString -> CSize
                              -> IO ()

{-
foreign import ccall "leveldb_writebatch_iterate"
  c_leveldb_writebatch_iterate :: Ptr LevelDB_Writebatch_t
-}

-- Iterators

foreign import ccall "leveldb_iter_valid"
  c_leveldb_iter_valid :: Ptr LevelDB_Iterator_t -> IO CUChar

foreign import ccall "leveldb_iter_seek_to_first"
  c_leveldb_iter_seek_to_first :: Ptr LevelDB_Iterator_t -> IO ()

foreign import ccall "leveldb_iter_seek_to_last"
  c_leveldb_iter_seek_to_last :: Ptr LevelDB_Iterator_t -> IO ()

foreign import ccall "leveldb_iter_seek"
  c_leveldb_iter_seek :: Ptr LevelDB_Iterator_t
                      -> CString -> CSize
                      -> IO ()

foreign import ccall "leveldb_iter_next"
  c_leveldb_iter_next :: Ptr LevelDB_Iterator_t -> IO ()

foreign import ccall "leveldb_iter_prev"
  c_leveldb_iter_prev :: Ptr LevelDB_Iterator_t -> IO ()

foreign import ccall "leveldb_iter_key"
  c_leveldb_iter_key :: Ptr LevelDB_Iterator_t -> Ptr CSize -> IO CString

foreign import ccall "leveldb_iter_value"
  c_leveldb_iter_value :: Ptr LevelDB_Iterator_t -> Ptr CSize -> IO CString

foreign import ccall "leveldb_iter_get_error"
  c_leveldb_iter_get_error :: Ptr LevelDB_Iterator_t -> Ptr CString -> IO ()


-- Approximate sizes

foreign import ccall "leveldb_approximate_sizes"
  c_leveldb_approximate_sizes :: Ptr LevelDB_t -> CInt
                              -> Ptr CString -> Ptr CSize
                              -> Ptr CString -> Ptr CSize
                              -> Ptr Word64 -> IO ()

-- Compacting

foreign import ccall "leveldb_compact_range"
  c_leveldb_compact_range :: Ptr LevelDB_t
                          -> CString -> CSize
                          -> CString -> CSize
                          -> IO ()

-- Properties

foreign import ccall "leveldb_property_value"
  c_leveldb_property_value :: Ptr LevelDB_t -> CString -> IO CString


