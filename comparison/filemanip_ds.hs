{-# LANGUAGE BangPatterns #-}
import System.Environment
import System.Posix.Files
import System.FilePath.Find hiding (fileSize)
import Debug.Trace

accum !sum info | isRegularFile stat = sum + fileSize stat
                | otherwise          = sum
  where stat = infoStatus info

main = do [dir] <- getArgs
          sum <- fold always accum 0 dir
          putStrLn$ "Total bytes: " ++ show sum
