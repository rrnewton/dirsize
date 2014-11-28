{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import System.Environment
-- import System.FilePath
-- import System.Directory
import System.Posix.Directory.ByteString
import System.Posix.Files.ByteString -- (getSymbolicLinkStatus)
import System.Posix.Directory.ByteString
import qualified Data.ByteString.Char8 as B
import Data.Int
import Data.Word

main = do [dir] <- getArgs
	  sum <- traverse (B.pack dir)
	  putStrLn$ "Total bytes: "++ show sum

type Scalar = Word64
-- type Scalar = COff

traverse :: B.ByteString -> IO Scalar
traverse path = do
  de   <- openDirStream path
  strm <- readDirStream de 
  sum  <- go de 0 strm
  closeDirStream de
  return sum
 where
    -- Empty string signals end of stream:
    go :: DirStream -> Scalar -> B.ByteString -> IO Scalar
    go d !sum ""   = return sum
    go d !sum "."  = readDirStream d >>= go d sum
    go d !sum ".." = readDirStream d >>= go d sum
    go d !sum  x   = 
--       do let newpath = path </> x
       do let newpath = path `B.append` "/" `B.append` x      
	  stat <- getSymbolicLinkStatus newpath 
	  if isDirectory stat 
	   then do !n   <- traverse newpath 
		   strm <- readDirStream d 
		   go d (n + sum) strm
	   else do 
		   let newsum = if isRegularFile stat 
				then sum + fromIntegral (fileSize stat)
				else sum
		   next <- readDirStream d 
		   go d newsum next		 
