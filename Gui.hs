--module Gui(run_gui)
--where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo

import Data.Maybe(fromMaybe)
import Control.Monad(unless)

import Control.Concurrent(yield)
import Control.Concurrent.MVar
import Data.IORef

import Data.Complex
import Numeric.Transform.Fourier.FFT

import Data.Array.IArray(amap,elems,bounds)
import Data.Array.MArray(writeArray)

import Data.Word(Word16,Word8)

import Capturer



glade_path = "gui.glade"

data Switch = Started | Stopped

run_gui = do
  initGUI
  -- reading xml
  xmlM <- xmlNewWithRootAndDomain glade_path (Just "window_analizer") Nothing
  let xml = fromMaybe (error "cant open xml gui description!") xmlM
  -- main window
  window_analizer <- xmlGetWidget xml castToWindow "window_analizer"
  -- canvas
  drawing_area <- xmlGetWidget xml castToDrawingArea "drawing_area"
  -- buttons
  button_start_stop <- xmlGetWidget xml castToButton "button_start_stop"

  ray_pos <- newIORef 10 :: IO (IORef Int)

  (start, stop) <- run_capturer $! draw_sound drawing_area ray_pos

  switch <- newMVar Stopped
  let start_stop = modifyMVar_ switch $ \st ->
                     case st of
                       Stopped -> do start
                                     buttonSetLabel button_start_stop "Stop"
                                     return Started
                       Started -> do stop
                                     buttonSetLabel button_start_stop "Start"
                                     return Stopped

  onClicked button_start_stop start_stop

  onDestroy window_analizer mainQuit

  widgetShowAll window_analizer
  timeoutAdd (yield >> return True) 50
  mainGUI

draw_sound :: DrawingArea -> (IORef Int) -> Samples -> IO ()
draw_sound da pos_ref samples = do
  (width, height) <- drawingAreaGetSize da
  let comp_samples = amap (\s -> (fint s) :+ 0 :: Complex Double) samples
      freqs = amap ((min 255) . round .(/256). magnitude) $ fft comp_samples
      freq_list = elems freqs :: [Word8]
      freq_len = length freq_list
      meaningful = freq_len `div` 2
      peak = maximum freq_list

  ray_pos <- readIORef pos_ref

  drawable <- drawingAreaGetDrawWindow da
  gc <- gcNew drawable

  pb <- pixbufNew ColorspaceRgb False 8 1 meaningful
  pixels <- pixbufGetPixels pb :: IO (PixbufData Int Word8)
  stride <- pixbufGetRowstride pb
  unless (stride == 4) $ error "unsupported format!"
  let set_point (y,c) = do
        writeArray pixels (stride*y) c
        writeArray pixels (stride*y + 1) c
        writeArray pixels (stride*y + 2) c
        writeArray pixels (stride*y + 3) 0

  mapM_ set_point $ zip (reverse [0 .. meaningful-1]) freq_list

  scaled_to_fit <- pixbufScaleSimple pb 1 height InterpBilinear
  drawPixbuf drawable gc scaled_to_fit 0 0 ray_pos 0 1 height RgbDitherNone (-1) (-1)

  writeIORef pos_ref $ if ray_pos >= width - 1 then 0 else ray_pos + 1


main = run_gui