--module Gui(run_gui)
--where

import Graphics.UI.Gtk hiding (fill)
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo

import Data.Maybe(fromMaybe)

import Control.Concurrent(yield)
import Control.Concurrent.MVar

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

  (start, stop) <- run_capturer $! draw_sound drawing_area

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
  timeoutAdd (yield >> return True) 100
  mainGUI

draw_sound :: DrawingArea -> [Int16] -> IO ()
draw_sound da samples = do
  (width', height') <- drawingAreaGetSize da
  let width = realToFrac width'
      height = realToFrac height'
      num_samples = length samples
  drawable <- drawingAreaGetDrawWindow da
  renderWithDrawable drawable $!
    do -- clearing
       setSourceRGB 1 1 1
       rectangle 0 0 width height
       fill
       -- drawing
       setSourceRGB 0 0 0
       setLineWidth 1
       moveTo 0 (height/2)
       mapM_ (uncurry lineTo) $!
             zip [0, width / fint num_samples .. width]
                 (map (\s -> height/2 - fint s * height / 65536) samples)
       stroke

main = run_gui