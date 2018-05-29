{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent.Async as A
import System.Posix.Files as F
import System.Process
import System.Environment
import Turtle as T
import Data.Text as Text
import Prelude as P
    
-- Given a list of files, build a directory 

type Acc = [(Int,Text)]

reduce :: Acc -> Line -> IO Acc
reduce !acc ln = do
  stat <- getFileStatus (Text.unpack (lineToText ln))
  return $! (fromIntegral (F.fileSize stat),
             lineToText ln )
            : acc
    
main :: IO ()
main = do
  let findem dir = inproc "find" [dir] mempty
      myfold = (FoldM reduce (return []) return)
  putStrLn "hello"

  [d1,d2] <- getArgs
           
  a <- A.async $ T.foldIO (findem (Text.pack d1)) myfold
  b <- A.async $ T.foldIO (findem (Text.pack d2)) myfold
  (l1,l2) <- A.waitBoth a b

  print $ P.take 10 l1
  print $ P.length  l1
  print $ P.length  l2
