{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (threadDelay)    
import Control.Exception (evaluate)
import Control.Concurrent.Async as A
-- import Data.IORef as R
import Data.Atomics.Counter as C -- For unboxed counter.
import Data.IntMap        as M
import System.Posix.Files as F
import System.Process     as SP
import System.Environment
import Turtle             as T
import Data.Text          as Text
import Prelude            as P

import System.Console.Concurrent as CC
import System.Console.Regions

type Acc = M.IntMap (Text,FileStatus)

-- Non-threadsafe bump operation.
bump :: AtomicCounter -> IO ()
bump c = do n <- C.readCounter c
            C.writeCounter c (n+1)
    
reduce :: AtomicCounter -> Acc -> Line -> IO Acc
reduce ref !acc ln = do
--  stat <- getFileStatus (Text.unpack (lineToText ln))
  stat <- getSymbolicLinkStatus (Text.unpack (lineToText ln))
  bump ref
  return $! M.insert (fromIntegral (F.fileSize stat))
                     (lineToText ln, stat)
                     acc

statusPrint :: AtomicCounter -> AtomicCounter -> Async a -> IO ()
statusPrint c1 c2 a = spawnem >> return ()
  where
  spawnem = displayConsoleRegions $ 
    withConsoleRegion Linear (\r -> setConsoleRegion r hdr >>
                                    loop c1 r)
      `concurrently`
    withConsoleRegion Linear (\r -> setConsoleRegion r hdr >>
                                    loop c2 r)
    
  hdr = ("Paths found: "::String)
  loop cnt r = do
     x <- A.poll a
     n <- C.readCounter cnt
     case x of
       Nothing -> do setConsoleRegion r (hdr++show n)
                     threadDelay 300000
                     loop cnt r
       Just _  -> do n <- C.readCounter cnt
                     finishConsoleRegion r ("Final found: "++show n)
                     return ()
              
main :: IO ()
main = do
  let findem dir = inproc "find" [dir] mempty
      myfold r = (FoldM (reduce r) (return M.empty) return)
  [d1,d2] <- getArgs
           
  count1 <- C.newCounter 0
  count2 <- C.newCounter 0
             
  a <- A.async $ T.foldIO (findem (Text.pack d1)) (myfold count1)
  b <- A.async $ T.foldIO (findem (Text.pack d2)) (myfold count2)

  both <- A.async (A.waitBoth a b)

  putStrLn "Scanning both input directories..."
  statusPrint count1 count2 both

  (m1,m2) <- A.wait both
  putStrLn "Computing intersection (based on file sizes)"  
  m3 <- evaluate $ M.intersectionWith (,) m1 m2
  putStrLn$ "Intersection size: "++show (M.size m3)

  mapM_ (\(sz,((nm1,_),(nm2,_))) -> do putStrLn (show sz ++ ": " ++ Text.unpack nm1)
                                       putStrLn ("         " ++ Text.unpack nm2))
        (M.toList m3)
  
  -- print $ P.length  l1
  -- print $ P.length  l2
  putStrLn "Done."


-- do { (r,a) <- startProgress (\_ -> "A") (\_ -> "B") 60 (Progress 0 100); let loop = (incProgress r 1 >> sleep 1 >> loop) in loop }        
