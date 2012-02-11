--module Gui(run_gui)
--where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo

import Graphics.UI.Gtk.Gdk.GC

import Data.Maybe(fromMaybe,fromJust)
import Control.Monad(unless,when,liftM)

import Control.Concurrent(yield,forkOS)
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Data.IORef

import Data.Complex

import Data.Array.IArray(amap,elems,bounds)
import Data.Array.MArray(writeArray)
import Data.Array.Base(unsafeWrite)
import Data.Word(Word16,Word8)

import Capturer
import FFTW

import Tones

forever a = a >> forever a

glade_path = "gui.glade"

data Switch = Started | Stopped

run_gui = do
  --initGUI
  unsafeInitGUIForThreadedRTS
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
  spinbutton_top_freq <- xmlGetWidget xml castToSpinButton "spinbutton_top_freq"
  spinbutton_to_read <- xmlGetWidget xml castToSpinButton "spinbutton_to_read"
  combobox_reference_tone <- xmlGetWidget xml castToComboBox "combobox_reference_tone"

  samples_queue <- newChan :: IO (Chan Samples)
  (start, stop) <- run_capturer $ writeChan samples_queue

  transformed_queue <- newChan :: IO (Chan Transformed)
  dummy_plan <- make_plan 512
  plan_var <- newMVar (fromJust dummy_plan) :: IO (MVar FFTW_Plan)
  forkOS $ fourier_trans plan_var samples_queue transformed_queue

  -- Drawing context
  let newDC :: Int -> Int -> Int -> IO DrawingContext
      newDC freq buf_halfsize stripe_len = do
      (width, height) <- widgetGetSize drawing_area
      dw <- widgetGetDrawWindow drawing_area

      ds <- pixbufNew ColorspaceRgb False 8 2 stripe_len -- scan line
      fc <- pixbufNew ColorspaceRgb False 8 width height -- in memory canvas
      rp <- newIORef 0

      return $! DC {
                 drawing_window = dw,
                 cb_ref_tone = combobox_reference_tone,
                 data_stripe = ds,
                 full_canvas = fc,
                 ray_pos = rp,
                 frequency = freq,
                 buffer_halfsize = buf_halfsize,
                 stripe_length = stripe_len,
                 canvas_size = (width, height) }

  dc <- newDC 8000 512 512
  dc_ref <- newIORef dc

  let draw_next = do
        empty <- isEmptyChan transformed_queue
        unless empty $ do
          freqs <- readChan transformed_queue
          dc <- readIORef dc_ref
          draw_sound dc freqs
          draw_next

  timeoutAddFull (draw_next >> return True) priorityLow 20

  onExposeRect drawing_area $ redraw_rect dc_ref

  switch <- newMVar Stopped
  let start_stop = modifyMVar_ switch $ \st ->
                     case st of
                       Stopped -> do freq <- spinButtonGetValueAsInt spinbutton_freq
                                     resolution <- spinButtonGetValueAsInt spinbutton_to_read
                                     top_freq <- spinButtonGetValueAsInt spinbutton_top_freq
                                     let buf_halfsize = 1 + resolution `div` 2
                                         stripe_len = freq_to_index freq buf_halfsize (fromIntegral top_freq)
                                     widgetSetSizeRequest drawing_area 700 stripe_len
                                     dc <- newDC freq buf_halfsize stripe_len
                                     writeIORef dc_ref dc
                                     plan <- liftM (fromMaybe (error "cant create plan")) $ make_plan resolution
                                     takeMVar plan_var
                                     putMVar plan_var plan
                                     start freq resolution
                                     buttonSetLabel button_start_stop "Stop capturing"
                                     return Started
                       Started -> do stop
                                     buttonSetLabel button_start_stop "Start capturing"
                                     return Stopped

  --on_resize :: DrawingArea -> IORef DrawingContext -> Allocation -> IO ()
  let on_resize dc_ref rect = do
        dc <- readIORef dc_ref
        pos <- readIORef (ray_pos dc)
        dc' <- newDC (frequency dc) (buffer_halfsize dc) (stripe_length dc)
        when ( pos < (fst . canvas_size) dc') $ do
          writeIORef (ray_pos dc') pos
          writeIORef dc_ref dc'

  onClicked button_start_stop start_stop

  afterSizeAllocate drawing_area $ on_resize dc_ref

  onDestroy window_analizer mainQuit

  widgetShowAll window_analizer
  mainGUI

type Transformed = [Word8]

fourier_trans :: MVar FFTW_Plan -> Chan Samples -> Chan Transformed -> IO ()
fourier_trans var input output = forever $ do
  samples <- readChan input
  let samples' = map fint $ elems samples :: [Double]
      n = length samples'
      sine = map (\k -> sin $ fromIntegral k / fromIntegral n * pi) [0..n-1]
      samples'' = zipWith(*) sine samples'
  comp_list <- withMVar var $ \plan -> execute plan samples''
  let freqs = map (round . (min 255) . (/128). magnitude) comp_list :: [Word8]
  writeChan output freqs

-- Converts a frequency to an array index as returned by FFTW,
-- takes discretisation frequency and buffer size divided by two
-- (signal is real, so second half of returned values is redundand)
freq_to_index :: Int -> Int -> Double -> Int
freq_to_index freq0 buf_halfsize f =
    round $ (2 * f) * (fromIntegral buf_halfsize) / (fromIntegral $ freq0)

data DrawingContext = DC { drawing_window :: DrawWindow,
                           cb_ref_tone :: ComboBox,
                           data_stripe :: Pixbuf,
                           full_canvas :: Pixbuf,
                           ray_pos :: IORef Int,
                           frequency :: Int,
                           buffer_halfsize :: Int,
                           stripe_length :: Int,
                           canvas_size :: (Int,Int)
                         }

draw_sound :: DrawingContext -> Transformed -> IO ()
draw_sound dc freqs = do
  let (width, height) = canvas_size dc
      stripe_len = stripe_length dc
  pos <- readIORef $ ray_pos dc

  let ds = data_stripe dc
  pixels <- pixbufGetPixels ds :: IO (PixbufData Int Word8)
  stride <- pixbufGetRowstride ds
  unless (stride == 8) $ error "unsupported format!"
  let set_point (y,c) = do
        set_color y 0 (c,c,c)         -- spectrum
        set_color y 3 (255,0,0)       -- marker

      set_color y d (r,g,b) = do
        unsafeWrite pixels (stride*y + d) r
        unsafeWrite pixels (stride*y + d+1) g
        unsafeWrite pixels (stride*y + d+2) b

  mapM_ set_point $ zip [stripe_len-1, stripe_len-2 .. 0] freqs

  -- Drawing ref tone
  rt <- comboBoxGetActive $ cb_ref_tone dc

  let draw_freq f c = do
        let n = freq_to_index (frequency dc) (buffer_halfsize dc) f
        when (n >= 0 && n < stripe_len) $ do
          set_color (stripe_len - n) 0 c -- origin is on top

      draw_pitch f = do
        let bound = (halfstep ** 0.5)
        draw_freq  f          (0,255, 0)
        draw_freq (f / bound) (0,0, 255)
        draw_freq (f * bound) (0,0, 255)

  unless (rt == -1) $ do
    let pitch = c1 * halfstep ^ rt
    mapM_ (\octave -> draw_pitch (pitch * 2**octave)) [-1..3]

  scaled_to_fit <- pixbufScaleSimple ds 2 height InterpBilinear
  let canvas = full_canvas dc
  pixbufCopyArea scaled_to_fit 0 0 (min 2 $ width - pos) height
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

main = run_gui