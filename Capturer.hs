module Capturer(run_capturer, fint, Int16)
where

import Sound.OpenAL
import Data.Maybe (fromMaybe)
import Control.Monad (liftM,when)
import Control.Concurrent (forkIO,threadDelay)
import Data.Array.Storable (StorableArray, newArray, withStorableArray,getElems,readArray)
import Data.Int(Int16)
import Control.Concurrent.MVar

buf_size = 128*1024

fint :: (Num b, Integral a) => a -> b
fint = fromIntegral

run_capturer :: ([Int16] -> IO ()) -> IO (IO (), IO ())
run_capturer act = do
  mutex <- newMVar ()
  takeMVar mutex
  dev <- open_capture_device 8000 buf_size
  tid <- forkIO (reader_thread dev mutex act)
  let start_cap = do
        captureStart dev
        putMVar mutex ()
      stop_cap = do
        captureStop dev
        takeMVar mutex
  return (start_cap, stop_cap)

max_to_read = 1024
reader_thread :: Device -> MVar () -> ([Int16] -> IO ()) -> IO ()
reader_thread dev mutex act = do
  arr <- newArray (0, max_to_read - 1) 0 :: IO (StorableArray Int Int16)
  forever $! do
    withMVar mutex $ \_ -> do
      samples_ready <- get $! captureNumSamples dev
      when (samples_ready > 0) $! do
        let to_read = min samples_ready (fint max_to_read)
        withStorableArray arr $! \ptr -> do
          captureSamples dev ptr to_read
        samples <- getElems arr
        act samples
        print samples_ready
    threadDelay 50000

forever a = a >> forever a

open_capture_device :: Frequency -> Int -> IO Device
open_capture_device freq num_samples = do
  dev <- liftM (fromMaybe $ error "can't open device") (openDevice Nothing)
  context <- liftM (fromMaybe $ error "can't create context") (createContext dev [])
  currentContext $= Just context
  liftM (fromMaybe $ error "can't capture open device")
        (captureOpenDevice Nothing freq Mono16 $ fint num_samples)
