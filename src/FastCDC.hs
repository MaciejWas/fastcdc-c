{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module FastCDC (chunkGenerator, chunkGeneratorAsync, ChunkSizeParams (..)) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Monad (void)
import Data.Word (Word32)
import Foreign (FunPtr, Ptr, malloc)
import Foreign.C (CSize (..), CString, newCString)
import Data.IORef (newIORef, modifyIORef, readIORef)

foreign import ccall "wrapper"
  wrapReporter ::
    ReportChunk ->
    IO (FunPtr ReportChunk)

foreign import ccall "wrapper"
  wrapComputer ::
    ComputeChunk ->
    IO (FunPtr ComputeChunk)

foreign import ccall "fastcdc_stream"
  chunkGeneratorC ::
    CString ->
    Word32 ->
    Word32 ->
    Word32 ->
    FunPtr (Ptr () -> CSize -> CSize -> IO ()) ->
    Ptr () ->
    IO ()

data ChunkSizeParams = ChunkSizeParams
  { minSize :: Word32,
    avgSize :: Word32,
    maxSize :: Word32
  }

type ReportChunk = Ptr () -> CSize -> CSize -> IO ()

type ComputeChunk = Ptr Char -> CSize -> IO ()

chunkGenerator ::
  (Integral i) =>
  ChunkSizeParams ->
  FilePath ->
  (i -> i -> IO ()) ->
  IO ()
chunkGenerator
  (ChunkSizeParams {minSize, avgSize, maxSize})
  filepath
  chunkReporter = do
    cpath <- newCString filepath
    ptr <- malloc :: IO (Ptr ())

    chunkReporterFnPtr <-
      wrapReporter
        (\_ start size -> chunkReporter (fromIntegral start) (fromIntegral size))

    chunkGeneratorC
      cpath
      minSize
      avgSize
      maxSize
      chunkReporterFnPtr
      ptr

chunkGeneratorSync ::
  ChunkSizeParams ->
  FilePath ->
  IO [(Int, Int)]
chunkGeneratorSync params filepath = do
  ref <- newIORef []
  let report offset size = modifyIORef ref (\r -> (offset,size):r )
  chunkGenerator params filepath report
  readIORef ref

-- | The returned channel yields tuples of offset and size
--  until the final chunk is reached. After that `Nothing` is returned
chunkGeneratorAsync ::
  ChunkSizeParams ->
  FilePath ->
  IO (Chan (Maybe (Int, Int)))
chunkGeneratorAsync
  params
  filepath = do
    channel <- newChan
    let report start size =
          writeChan
            channel
            (Just (fromIntegral start, fromIntegral size))
    (void . forkIO)
      ( chunkGenerator
          params
          filepath
          report
          >> writeChan channel Nothing
      )
    return channel

chunkComputerAsync ::
  ChunkSizeParams ->
  FilePath ->
  IO (Chan (Maybe (Int, Int)))
chunkComputerAsync
  params
  filepath = do
    channel <- newChan
    let report start size =
          writeChan
            channel
            (Just (fromIntegral start, fromIntegral size))
    (void . forkIO)
      ( chunkGenerator
          params
          filepath
          report
          >> writeChan channel Nothing
      )
    return channel