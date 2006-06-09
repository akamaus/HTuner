--module Gui(run_gui)
--where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo

import Data.Maybe(fromMaybe)
import Control.Monad(unless,when)

import Control.Concurrent(yield,forkOS)
import Control.Concurrent.MVar
import Data.IORef
import Control.Concurrent.Chan

import Data.Complex
import Numeric.Transform.Fourier.FFT

import Data.Array.IArray(amap,elems,bounds)
import Data.Array.MArray(writeArray)

import Data.Word(Word16,Word8)

import Capturer

forever a = a >> forever a

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
  -- spin buttons
  spinbutton_freq <- xmlGetWidget xml castToSpinButton "spinbutton_freq"
  spinbutton_to_read <- xmlGetWidget xml castToSpinButton "spinbutton_to_read"

  samples_queue <- newChan :: IO (Chan Samples)
  (start, stop) <- run_capturer $ writeChan samples_queue

  transformed_queue <- newChan :: IO (Chan Transformed)
  forkOS $ fourier_trans samples_queue transformed_queue

  dc <- newDC drawing_area 512
  dc_ref <- newIORef dc

  let draw_next = do
        empty <- isEmptyChan transformed_queue
        unless empty $ do
          freqs <- readChan transformed_queue
          dc <- readIORef dc_ref
          draw_sound dc freqs

  timeoutAddFull (draw_next >> return True) priorityLow 20

  onExposeRect drawing_area $ redraw_rect dc_ref

  switch <- newMVar Stopped
  let start_stop = modifyMVar_ switch $ \st ->
                     case st of
                       Stopped -> do freq <- spinButtonGetValueAsInt spinbutton_freq
                                     to_read <- spinButtonGetValueAsInt spinbutton_to_read
                                     widgetSetSizeRequest drawing_area 800 (to_read `div` 2)
                                     dc <- newDC drawing_area (to_read `div` 2)
                                     writeIORef dc_ref dc
                                     start freq to_read
                                     buttonSetLabel button_start_stop "Stop capturing"
                                     return Started
                       Started -> do stop
                                     buttonSetLabel button_start_stop "Start capturing"
                                     return Stopped

  onClicked button_start_stop start_stop

  afterSizeAllocate drawing_area $ on_resize drawing_area dc_ref

  onDestroy window_analizer mainQuit

  widgetShowAll window_analizer
  mainGUI

type Transformed = [Word8]

fourier_trans :: Chan Samples -> Chan Transformed -> IO ()
fourier_trans input output = forever $! do
  samples <- readChan input
  let samples' = amap (\s -> fint s :: Double) samples
      freqs = amap ((min 255) . round .(/128). magnitude) $ rfft samples'
      freq_list = elems freqs :: [Word8]
      half = let (a,b) = bounds samples in (b - a + 1) `div` 2
  writeChan output $! take half freq_list

data DrawingContext = DC { drawing_window :: DrawWindow,
                           data_stripe :: Pixbuf,
                           full_canvas :: Pixbuf,
                           ray_pos :: IORef Int,
                           stripe_length :: Int,
                           canvas_size :: (Int,Int)
                         }

newDC :: DrawingArea -> Int -> IO DrawingContext
newDC da stripe_len = do
  (width, height) <- drawingAreaGetSize da
  dw <- drawingAreaGetDrawWindow da

  ds <- pixbufNew ColorspaceRgb False 8 2 stripe_len
  fc <- pixbufNew ColorspaceRgb False 8 width height
  rp <- newIORef 0

  return $! DC dw ds fc rp stripe_len (width, height)

draw_sound :: DrawingContext -> Transformed -> IO ()
draw_sound dc freqs = do
  let (width, height) = canvas_size dc
      meaningful = stripe_length dc

  pos <- readIORef $ ray_pos dc

  let ds = data_stripe dc
  pixels <- pixbufGetPixels ds :: IO (PixbufData Int Word8)
  stride <- pixbufGetRowstride ds
  unless (stride == 8) $ error "unsupported format!"
  let set_point (y,c) = do
        -- spectrum
        writeArray pixels (stride*y) c
        writeArray pixels (stride*y + 1) c
        writeArray pixels (stride*y + 2) c
        -- marker
        writeArray pixels (stride*y + 3) 255
        writeArray pixels (stride*y + 4) 0
        writeArray pixels (stride*y + 5) 0

  mapM_ set_point $ zip (reverse [0 .. meaningful-1]) freqs

  scaled_to_fit <- pixbufScaleSimple ds 2 height InterpBilinear
  let canvas = full_canvas dc

  pixbufCopyArea scaled_to_fit 0 0 height (min 2 $ width - pos)  -- BUG in GTK2HS!!! width and height are swapped
                 canvas pos 0

  writeIORef (ray_pos dc) $ if pos >= width - 1 then 0 else pos + 1

  let dw = drawing_window dc
  dirty <- regionRectangle $ Rectangle pos 0 2 height
  visible <- drawableGetVisibleRegion dw
  regionIntersect dirty visible
  drawWindowInvalidateRegion dw dirty False

redraw_rect :: IORef DrawingContext -> Rectangle -> IO ()
redraw_rect dc_ref rect = do
  dc <- readIORef dc_ref
  let Rectangle x y w h = rect
      dw = drawing_window dc
      stf = full_canvas dc
  gc <- gcNew dw
  drawPixbuf dw gc stf x y x y w h RgbDitherNone w h

on_resize :: DrawingArea -> IORef DrawingContext -> Allocation -> IO ()
on_resize da dc_ref rect = do
  dc <- readIORef dc_ref
  let sl = stripe_length dc
  pos <- readIORef (ray_pos dc)
  dc' <- newDC da sl
  when ( pos < (fst . canvas_size) dc') $
         writeIORef (ray_pos dc') pos
  writeIORef dc_ref dc'

main = run_gui