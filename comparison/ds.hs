import System.Environment
import System.Directory.Tree
import System.PosixCompat.Files
import Data.Foldable    as F
main = do [dir] <- getArgs
	  let read file = 
	       do stat <- getSymbolicLinkStatus file
		  return (if isRegularFile stat 
			  then fileSize stat
			  else 0)
	  root :/ sizes  <- readDirectoryWithL read dir  
	  let bytes = F.foldr (+) 0 sizes
	  putStrLn$ "Contains "++ show bytes ++"  bytes." 
