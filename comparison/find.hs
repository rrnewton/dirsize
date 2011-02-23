import System.Directory
import System.FilePath
import System.Posix
import System.Environment

list path = do
  de <- openDirStream path
  readDirStream de >>= go de
  closeDirStream de
  where
    go d []   = return ()
    go d "."  = readDirStream d >>= go d
    go d ".." = readDirStream d >>= go d
    go d x = let newpath = path </> x
	     in do
	      e <- doesDirectoryExist newpath
	      if e then
	            list newpath >> readDirStream d >>= go d
	       else putStrLn newpath >> readDirStream d >>= go d 

main = 
 do [dir] <- getArgs
    list dir
