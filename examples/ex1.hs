module Main
       ( main -- :: IO ()
       ) where

import Control.Monad (liftM)
import System.Posix.User (getEffectiveUserID)
import Database.LevelDB as LDB

main :: IO ()
main = do
  dbname <- (("/tmp/leveldb_hs_test-"++) . show) `liftM` getEffectiveUserID
  say $ "creating db " ++ dbname

  let opts = defaultDBOptions{dbCreateIfMissing = True}
  db <- LDB.open opts dbname
  say "checking stats"
  case db of
  	Left e  -> putStrLn $ "err: " ++ e
  	Right r -> do
  	  say "stats"
	  p <- LDB.property r "leveldb.stats"
  	  print p
	  LDB.close r

say :: String -> IO ()
say x = putStrLn $ "===== " ++ x ++ " ====="
