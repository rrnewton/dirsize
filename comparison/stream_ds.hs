{-# LANGUAGE DoAndIfThenElse #-}

-- The idea of this one is to enumerate files in a list/stream and
-- process them in order.

import Prelude     as P hiding (concatMap) 
import Data.Stream as S
import System.Directory.Tree
import System.Environment


import Control.Monad
import System.Posix.Files
import System.Directory

empty = stream []

-- enumerateFiles :: String -> IO (Stream String)
-- enumerateFiles path = 
--    do ls <- getDirectoryContents path

--       (flip concatMap) (stream ls) $ \ childpath -> do 

-- 	stat <- getSymbolicLinkStatus childpath
-- 	if isDirectory stat then do
-- 	   enumerateFiles childpath
-- 	else if isRegularFile stat then 
-- 	   P.return (S.return childpath)
-- 	else 
--            P.return empty

-- TODO: Unsafe performIO...
enumerateFiles :: String -> IO (Stream FileStatus)
enumerateFiles path = loop "." (S.return path)
 where 
--  loop :: String -> Stream String -> IO (Stream String)
  loop root strm | S.null strm = P.return empty
  loop root strm | otherwise =
   do let path = S.head strm 
	  rest = S.tail strm
	  absolute = root++"/"++path

      if path == "." || path == ".." then
        loop root rest

      else do 
--	putStrLn$ "Processing root/path "++ root++" "++path
	stat <- getSymbolicLinkStatus absolute

	if isDirectory stat then do
	   ls <- getDirectoryContents absolute
--	   putStrLn$ "  got path contents: "++ show ls
	   a <- loop absolute (stream ls)
	   b <- loop root rest
	   P.return (a `append` b)

	else if isRegularFile stat then do
	   rest <- loop root rest
	   P.return (cons stat rest)

	else 
	   loop root rest

	 

main = 
 do [dir] <- getArgs
    strm <- enumerateFiles dir
    let sum = S.sum$ S.map fileSize strm
--    putStrLn$ "Length: "++show (S.length strm)
    putStrLn$ "Sum: "++ show sum
