module Capturer(run_capturer, fint, Int16, Samples)
where

import Sound.OpenAL
import Data.Maybe (fromMaybe)
import Control.Monad (liftM,when)
import Control.Concurrent (forkOS,threadDelay)
import Data.Array.Storable (StorableArray, newArray, withStorableArray,getElems,readArray,freeze)
import Data.Array(Array)
import Data.Int(Int16)
import Control.Concurrent.MVar

type Samples = Array Int Int16

buf_size = 128*1024

fint :: (Num b, Integral a) => a -> b
fint = fromIntegral

run_capturer :: (Samples -> IO ()) -> IO (IO (), IO ())
run_capturer act = do
  mutex <- newMVar ()
  takeMVar mutex
  dev <- open_capture_device 11025 buf_size
  tid <- forkOS (reader_thread dev mutex act)
  let start_cap = do
        captureStart dev
        putMVar mutex ()
      stop_cap = do
        captureStop dev
        takeMVar mutex
  return (start_cap, stop_cap)

to_read = 512
reader_thread :: Device -> MVar () -> (Samples -> IO ()) -> IO ()
reader_thread dev mutex act = do
  arr <- newArray (0, to_read - 1) 0 :: IO (StorableArray Int Int16)
  forever $! do
    withMVar mutex $ \_ -> do
      samples_ready <- get $! captureNumSamples dev
      if (samples_ready >= fint to_read) then
        withStorableArray arr $! \ptr -> do
          captureSamples dev ptr $ fint to_read
          samples <- freeze arr
          act samples
       else
        threadDelay 50000

forever a = a >> forever a

open_capture_device :: Frequency -> Int -> IO Device
open_capture_device freq num_samples = do
  dev <- liftM (fromMaybe $ error "can't open device") (openDevice Nothing)
  context <- liftM (fromMaybe $ error "can't create context") (createContext dev [])
  currentContext $= Just context
  liftM (fromMaybe $ error "can't capture open device")
        (captureOpenDevice Nothing freq Mono16 $ fint num_samples)
