module Main
where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo

import Data.Maybe(fromMaybe,fromJust)
import Control.Monad(unless,when,liftM)

import Control.Concurrent(yield,forkOS)
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Data.IORef

import Data.Complex

import Data.Array.IArray(amap,elems,bounds)
import Data.Array.MArray(writeArray)

import Data.Word(Word16,Word8)

import Capturer
import FFTW

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
  spinbutton_energy <- xmlGetWidget xml castToSpinButton "spinbutton_energy"


  samples_queue <- newChan :: IO (Chan Samples)
  (start, stop) <- run_capturer $ writeChan samples_queue

  transformed_queue <- newChan :: IO (Chan Transformed)
  dummy_plan <- make_plan 512
  plan_var <- newMVar (fromJust dummy_plan) :: IO (MVar FFTW_Plan)
  desired_energy_var <- newMVar init_energy_level :: IO (MVar Double)
  forkOS $ fourier_trans desired_energy_var plan_var samples_queue transformed_queue

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
                                     resolution <- spinButtonGetValueAsInt spinbutton_to_read
                                     let stripe_len = 1 + resolution `div` 2
                                     widgetSetSizeRequest drawing_area 800 stripe_len
                                     dc <- newDC drawing_area stripe_len
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

  onClicked button_start_stop start_stop

  onValueSpinned spinbutton_energy $ do
    val <- spinButtonGetValue spinbutton_energy
    takeMVar desired_energy_var
    putMVar desired_energy_var val

  afterSizeAllocate drawing_area $ on_resize drawing_area dc_ref

  onDestroy window_analizer mainQuit

  widgetShowAll window_analizer
  mainGUI

type Transformed = [Word8]

init_gain = 1/100
gain_step = 1.05
init_energy_level = 10 :: Double

fourier_trans :: MVar Double -> MVar FFTW_Plan -> Chan Samples -> Chan Transformed -> IO ()
fourier_trans desir_energy_var var input output = do
  --gain_ref <- newIORef init_gain
  forever $ do
    samples <- readChan input
    let samples' = map fint $ elems samples :: [Double]
        shadow = map (\k -> exp $ -8 * (fint k / fint (length samples') - 0.5)^2 ) [0..]
        shadowed = zipWith (*) samples' shadow
    comp_list <- withMVar var $ \plan -> execute plan shadowed
    --gain <- readIORef gain_ref
    abate <- readMVar desir_energy_var
    let freqs = map ((/abate) . log . magnitude) comp_list
        --sharpen = zipWith (\x y -> abs (x - y)) freqs (0:freqs)
        low = max 0 (minimum freqs)
        high = maximum freqs
        a = 255 / (high - low)
        b = -a * low
        bounded = map (\y -> round $ a*y + b)  freqs -- sharpen
    --print freqs
    print $ maximum bounded
   {-     energy = sum (map fint bounded) / fint (length freqs)
    print energy
    withMVar desir_energy_var $ \energy_level ->
      if energy < energy_level then writeIORef gain_ref (gain * gain_step)
       else writeIORef gain_ref (gain / gain_step) -}
    writeChan output bounded

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