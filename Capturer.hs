module Capturer(run_capturer, fint, Int16, Samples)
where

import Sound.OpenAL
import Data.Maybe (fromMaybe)
import Control.Monad (liftM,when,unless)
import Control.Concurrent (forkOS,threadDelay)
import Data.Array.Storable (StorableArray, newArray, withStorableArray,getElems,readArray,freeze)
import Data.Array(Array)
import Data.Int(Int16)
import Control.Concurrent.MVar

type Samples = Array Int Int16

loop_buf_size = 128*1024

fint :: (Num b, Integral a) => a -> b
fint = fromIntegral

run_capturer :: (Samples -> IO ()) -> IO (Int -> Int -> IO (), IO ())
run_capturer act = do
  prepare_playback_device

  var <- newEmptyMVar :: IO (MVar (Device,Int))

  tid <- forkOS (reader_thread var act)

  let start_cap freq to_read = do
        dev <- open_capture_device (fint freq) loop_buf_size
        captureStart dev
        putMVar var (dev, to_read)

      stop_cap = do
        (dev,_) <- takeMVar var
        captureStop dev
        res <- captureCloseDevice dev
        unless res $ error "cant close device"

  return (start_cap, stop_cap)

reader_thread :: MVar (Device,Int) -> (Samples -> IO ()) -> IO ()
reader_thread var act = do
  forever $! do
    res <- withMVar var $ \(dev, to_read) -> do
      arr <- newArray (0, to_read - 1) 0 :: IO (StorableArray Int Int16)
      samples_ready <- get $! captureNumSamples dev
      if (samples_ready >= fint to_read) then
        withStorableArray arr $! \ptr -> do
          captureSamples dev ptr $ fint to_read
          samples <- freeze arr
          act samples
          return True
       else
          return False
    unless res $ threadDelay 50000

forever a = a >> forever a

prepare_playback_device :: IO Device
prepare_playback_device = do
  dev <- liftM (fromMaybe $ error "can't open device") (openDevice Nothing)
  context <- liftM (fromMaybe $ error "can't create context") (createContext dev [])
  currentContext $= Just context
  return dev

open_capture_device :: Frequency -> Int -> IO Device
open_capture_device freq num_samples =
  liftM (fromMaybe $ error "can't capture open device")
        (captureOpenDevice Nothing freq Mono16 $ fint num_samples)
