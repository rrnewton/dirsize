{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import System.Directory.Tree
import System.PosixCompat.Files
-- import System.Posix.Files

import Data.List.Split
import Data.List
import Data.Int
import Data.Traversable as T
import Data.Foldable    as F

main = do
--  argcv <- getArgs
  [dir] <- getArgs
  putStrLn$ "Reading directory: " ++ dir

  let read file = 
       do stat <- getSymbolicLinkStatus file
	  return (if isRegularFile stat 
		  then fileSize stat
		  else 0)
	    
--  stats <- readDirectoryWithL read dir  
--  root :/ (sizes :: DirTree Int64) <- readDirectoryWithL read dir  
  root :/ sizes  <- readDirectoryWithL read dir  

  let 
--      fn 0 x = x
--      fn 1 (cnt,bytes) = 
--      bytes = F.foldr fn (0,0) sizes
      bytes = F.foldr (+) 0 sizes


--  putStrLn$ "Found "++ show num ++" regular files." 
  putStrLn$ "Containing "++ commaint bytes ++"  bytes." 


-- I cannot *believe* there is not a standard call or an
-- easily-findable hackage library supporting locale-based printing of
-- numbers. [2011.01.28]
commaint :: Integral a => a -> String
commaint n = 
   reverse $
   Prelude.concat $
   intersperse "," $ 
   chunk 3 $ 
   reverse (show n)
