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
  alloca $ \ptr -> do
    withCString dbname $ \name -> do
      db' <- C.c_leveldb_open options name ptr
      r <- peek ptr
      ensure (r /= nullPtr)
      sfree ptr


  say "open"
  C.c_leveldb_options_set_create_if_missing options (fromIntegral 1)
  db <- alloca $ \ptr -> do
    withCString dbname $ \name -> do
      db' <- C.c_leveldb_open options name ptr
      r <- peek ptr
      ensure (r == nullPtr)
      checkGet db' roptions "foo" (nullPtr, 0)
      sfree ptr
      return db'


  say "put"
  alloca $ \ptr -> do
    withCStringLen "foo" $ \(kstr, klen) -> do
      withCStringLen "hello" $ \(vstr, vlen) -> do
        C.c_leveldb_put db woptions kstr (fromIntegral klen) vstr (fromIntegral vlen) ptr
        r <- peek ptr
        ensure (r == nullPtr)
        withCStringLen "hello" (checkGet db roptions "foo")
        sfree ptr
  

  say "writebatch"
  wb <- C.c_leveldb_writebatch_create
  withCStringLen "foo" $ \(k,kl) ->
    withCStringLen "a" $ \(v,vl) ->
      C.c_leveldb_writebatch_put wb k (fromIntegral kl) v (fromIntegral vl)
  C.c_leveldb_writebatch_clear wb

  withCStringLen "bar" $ \(k,kl) ->
    withCStringLen "b" $ \(v,vl) ->
      C.c_leveldb_writebatch_put wb k (fromIntegral kl) v (fromIntegral vl)

  withCStringLen "box" $ \(k,kl) ->
    withCStringLen "c" $ \(v,vl) ->
      C.c_leveldb_writebatch_put wb k (fromIntegral kl) v (fromIntegral vl)

  withCStringLen "bar" $ \(k,kl) ->
    C.c_leveldb_writebatch_delete wb k (fromIntegral kl)
  
  alloca $ \ptr -> do
    C.c_leveldb_write db woptions wb ptr
    r <- peek ptr
    ensure (r == nullPtr)

  withCStringLen "hello" (checkGet db roptions "foo")
  checkGet db roptions "bar" (nullPtr,0)
  withCStringLen "c" (checkGet db roptions "box")
  -- TODO FIXME: writebatch iterate support
  C.c_leveldb_writebatch_destroy wb


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
  snap <- C.c_leveldb_create_snapshot db
  alloca $ \ptr -> do
    withCStringLen "foo" $ \(str,len) -> (C.c_leveldb_delete db woptions str (fromIntegral len) ptr)
    r <- peek ptr
    ensure (r == nullPtr)
  C.c_leveldb_readoptions_set_snapshot roptions snap
  withCStringLen "hello" (checkGet db roptions "foo")
  C.c_leveldb_readoptions_set_snapshot roptions nullPtr
  checkGet db roptions "foo" (nullPtr, 0)
  C.c_leveldb_release_snapshot db snap


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
      checkGet db' roptions "foo" (nullPtr, 0)
      checkGet db' roptions "bar" (nullPtr, 0)
      withCStringLen "c" (checkGet db' roptions "box")
      return db'

  say "cleanup"
  C.c_leveldb_close db
  C.c_leveldb_options_destroy options
  C.c_leveldb_readoptions_destroy roptions
  C.c_leveldb_writeoptions_destroy woptions
  C.c_leveldb_cache_destroy cache
  C.c_leveldb_env_destroy env
  putStrLn "PASS"


-- Utils

checkGet :: Ptr LevelDB_t -> Ptr LevelDB_Readoptions_t -> String -> CStringLen -> IO ()
checkGet db ropts key (expected, elen) = 
  withCStringLen key $ \(cstr, clen) -> do
    alloca $ \vallen -> do
      alloca $ \err -> do
        ptr <- C.c_leveldb_get db ropts cstr (fromIntegral clen) vallen err
        ensure (err /= nullPtr)
        if (expected /= nullPtr) then do
          vlen <- peek vallen
          str  <- peekCStringLen (ptr, fromIntegral vlen)
          str2 <- peekCStringLen (expected, elen)
          ensure (str == str2)
         else do
          ensure (ptr == nullPtr)
        sfree err

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

say :: String -> IO ()
say x = putStrLn $ "===== " ++ x ++ " ====="
