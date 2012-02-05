-- Translation of 'c_test.c' in the LevelDB source code.
module Main
       ( main -- :: IO ()
       ) where

import Control.Monad (liftM, when)
import System.Posix.User (getEffectiveUserID)
import Database.LevelDB.FFI as C

import Foreign.C
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Exit

main :: IO ()
main = do
  dbname <- (("/tmp/leveldb_hs_test-"++) . show) `liftM` getEffectiveUserID
  say $ "creating db " ++ dbname

  say "create_objects"
  env   <- C.c_leveldb_create_default_env
  cache <- C.c_leveldb_cache_create_lru (fromIntegral 100000)

  options <- C.c_leveldb_options_create
--  C.c_leveldb_options_set_comparator options cmp
  C.c_leveldb_options_set_error_if_exists options (fromIntegral 1)
  C.c_leveldb_options_set_cache options cache
  C.c_leveldb_options_set_env options env
  C.c_leveldb_options_set_write_buffer_size options (fromIntegral 100000)
  C.c_leveldb_options_set_paranoid_checks options (fromIntegral 1)
  C.c_leveldb_options_set_max_open_files options (fromIntegral 10)
  C.c_leveldb_options_set_block_size options (fromIntegral 1024)
  C.c_leveldb_options_set_block_restart_interval options (fromIntegral 8)
  C.c_leveldb_options_set_compression options C.c_leveldb_no_compression

  roptions <- C.c_leveldb_readoptions_create
  C.c_leveldb_readoptions_set_verify_checksums roptions (fromIntegral 1)
  C.c_leveldb_readoptions_set_fill_cache roptions (fromIntegral 0)

  woptions <- C.c_leveldb_writeoptions_create
  C.c_leveldb_writeoptions_set_sync woptions (fromIntegral 1)

  say "destroy"
  alloca $ \ptr -> do
    withCString dbname $ \name -> do
      C.c_leveldb_destroy_db options name ptr
      sfree ptr

  say "open_error"
  db <- alloca $ \ptr -> do
    withCString dbname $ \name -> do
      db' <- C.c_leveldb_open options name ptr
      r <- peek ptr
      ensure (r /= nullPtr)
      sfree ptr
      return db'

  say "open"
  C.c_leveldb_options_set_create_if_missing options (fromIntegral 1)
  db <- alloca $ \ptr -> do
    withCString dbname $ \name -> do
      db' <- C.c_leveldb_open options name ptr
      r <- peek ptr
      ensure (r == nullPtr)
      sfree ptr
      return db'

  say "put"

  say "writebatch"

  say "iter"

  say "approximate_sizes"

  say "property"
  prop <- withCString "nosuchprop" (C.c_leveldb_property_value db)
  ensure (prop == nullPtr)
  prop <- withCString "leveldb.stats" (C.c_leveldb_property_value db)
  ensure (prop /= nullPtr)
--  prop' <- peekCString prop
--  putStrLn $ "stats:\n" ++ prop'
  free prop

  say "snapshot"

  say "repair"
  db <- alloca $ \ptr -> do
    withCString dbname $ \name -> do
      C.c_leveldb_close db
      C.c_leveldb_options_set_create_if_missing options (fromIntegral 0)
      C.c_leveldb_options_set_error_if_exists options (fromIntegral 0)
      C.c_leveldb_repair_db options name ptr
      r <- peek ptr
      ensure (r == nullPtr)
      db' <- C.c_leveldb_open options name ptr
      r <- peek ptr
      ensure (r == nullPtr)
      -- FIXME
      return db'

  say "cleanup"
  C.c_leveldb_close db
  C.c_leveldb_options_destroy options
  C.c_leveldb_readoptions_destroy roptions
  C.c_leveldb_writeoptions_destroy woptions
  C.c_leveldb_cache_destroy cache
  C.c_leveldb_env_destroy env
  return ()

ensure :: Bool -> IO ()
ensure True  = return ()
ensure False = do
  putStrLn "check failed!"
  exitWith (ExitFailure 1)

sfree :: Ptr CString -> IO ()
sfree p = do
  x <- peek p
  when (x /= nullPtr) $ do
    free x

say x = putStrLn $ "===== " ++ x ++ " ====="
