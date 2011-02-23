{-# LANGUAGE BangPatterns #-}
import System.Environment
import System.FilePath
import System.Directory
import System.Posix.Files
import System.Posix.Directory
import Data.Int

main = do [dir] <- getArgs
	  sum <- traverse dir
	  putStrLn$ "Total bytes: "++ show sum

traverse path = do
  de   <- openDirStream path
  strm <- readDirStream de 
  sum  <- go de 0 strm
  closeDirStream de
  return sum
 where
    -- Empty string signals end of stream:
    go d !sum ""   = return sum
    go d !sum "."  = readDirStream d >>= go d sum
    go d !sum ".." = readDirStream d >>= go d sum
    go d !sum  x   = 
       do let newpath = path </> x
	  stat <- getSymbolicLinkStatus newpath 
	  if isDirectory stat 
	   then do !n   <- traverse newpath 
		   strm <- readDirStream d 
		   go d (n + sum) strm
	   else do 
		   let newsum = if isRegularFile stat 
				then sum + (fileSize stat)
				else sum
		   next <- readDirStream d 
		   go d newsum next		 
