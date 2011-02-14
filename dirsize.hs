

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
  stats <- readDirectoryWithL getSymbolicLinkStatus dir  
--  stats <- readDirectoryWith getSymbolicLinkStatus dir  

  let 
      real_files = map (\ (x,y) -> (x, fileSize y)) $ 
	           filter (isRegularFile . snd) $
		   toList $ zipPaths stats
--      fn :: (String,Int64) -> (Int64,Int64) -> (Int64,Int64)
      fn (path,size) (n,bytes) = (n+1, bytes + size)

      (num,bytes) = F.foldr fn (0,0) real_files

  putStrLn$ "Found "++ show num ++" regular files." 
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
