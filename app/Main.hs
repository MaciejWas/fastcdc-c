{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import FastCDC (chunkGenerator, ChunkSizeParams(..))

import System.IO (stdin, openFile, IOMode(ReadMode))
import Control.Monad (forM_)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  (case args of
    (filename:minSizeArg:avgSizeArg:maxSizeArg:[]) -> (
      chunkGenerator 
        (ChunkSizeParams
          { minSize = (read minSizeArg),
            avgSize = (read avgSizeArg),
            maxSize = (read maxSizeArg)
          }))
        filename
        (\start size -> putStrLn ("Chunk found:\toffset=" ++ show start ++ "\tsize=" ++ show size))
    _ -> print "usage: fastcdc filename minSize avgSize maxSize"
    )
    