-- This version tests an existing library routine for enumerating files.

import Data.List
import Control.Monad
import System.Posix.Files
import System.Environment
import System.IO.HVFS	
import System.IO.HVFS.Utils  

size f | withStat f vIsRegularFile = withStat f vFileSize
       | otherwise                 = 0 

main = 
 do [dir] <- getArgs
    ls <- recurseDirStat SystemFS dir
    print$ foldl' (+) 0 (map (size . snd) ls)
