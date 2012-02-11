module Capturer(run_capturer, fint, Int16, Samples)
where

import Sound.OpenAL
import Data.Maybe (fromMaybe)
import Control.Monad (liftM,when,unless)
import Control.Concurrent (forkOS,threadDelay)
import Data.Array.Storable (StorableArray, newArray, withStorableArray,getElems,readArray,unsafeFreeze)
import Data.Array(Array, bounds, elems, listArray)
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
reader_thread var act = grab_chunks 2 []
 where
  grab_chunk num_chunks = do
    withMVar var $ \(dev, buf_size) -> do
      let to_read = buf_size `div` num_chunks
      grab_samples dev to_read
  grab_chunks n chunks = do
    ch <- grab_chunk n
    let chunks' = chunks ++ [ch]
    if length chunks < n-1 then
      grab_chunks n chunks'
     else do
      act $ sum_chunks chunks'
      grab_chunks n (tail chunks')


sum_chunks xs = let size = sum $ map ((\(a,b) -> b-a+1) . bounds) xs
                in listArray (0, size-1) $ concat $ map elems xs

-- Reads a given number of samples from audio device. Waits if necessary
grab_samples dev num = do
  samples_ready <- get $! captureNumSamples dev
  if (samples_ready >= fint num) then do
    arr <- newArray (0, num - 1) 0 :: IO (StorableArray Int Int16)
    withStorableArray arr $! \ptr -> do
      captureSamples dev ptr $ fint num
      unsafeFreeze arr
   else do
    threadDelay 10000
    grab_samples dev num

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
