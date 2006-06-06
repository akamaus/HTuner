import Sound.OpenAL
import Data.Maybe (fromMaybe)
import Control.Monad (liftM,when)
import Control.Concurrent (forkOS,threadDelay)
import Data.Array.Storable (StorableArray, newArray, withStorableArray,getElems,readArray)
import Data.Int(Int16)

buf_size = 128*1024

fint :: (Num b, Integral a) => a -> b
fint = fromIntegral

main = do
  dev <- open_capture_device 8000 buf_size
  captureStart dev
  forkOS (reader_thread dev)
  forever $ threadDelay 1000000

max_to_read = 1*1024
reader_thread :: Device -> IO ()
reader_thread dev = do
  arr <- newArray (0, max_to_read - 1) 0 :: IO (StorableArray Int Int16)
  forever $! do
    samples_ready <- get $! captureNumSamples dev
    when (samples_ready > 0) $! do
      let to_read = min samples_ready (fint max_to_read)
      withStorableArray arr $! \ptr -> do
        captureSamples dev ptr to_read
      samples <- getElems arr
      let level = (sum $ map (\s -> abs $ fint s :: Float ) samples) / fint to_read
      print (samples_ready, level)
    threadDelay 100000

forever a = a >> forever a

open_capture_device :: Frequency -> Int -> IO Device
open_capture_device freq num_samples = do
  dev <- liftM (fromMaybe $ error "can't open device") (openDevice Nothing)
  context <- liftM (fromMaybe $ error "can't create context") (createContext dev [])
  currentContext $= Just context
  liftM (fromMaybe $ error "can't capture open device")
        (captureOpenDevice Nothing freq Mono16 $ fint num_samples)
