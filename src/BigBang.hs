module BigBang
  (bigBang, Handlers(..), handlers, atopBG)
  where

import qualified Data.Text as Text

import Diagrams.Prelude (Diagram, SizeSpec, V2, P2,
                         dims2D, absolute, atop, withEnvelope, ( # ))
import Diagrams.Backend.Rasterific (Rasterific, B)
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.Backend.Rasterific as Dia

import Graphics.UI.WX (Var, varCreate, varGet, varSet,
                       frameFixed, panel, timer, Prop((:=)),
                       layout, space, on, paint, interval, set, command, click, widget, enabled, repaint)
import qualified Graphics.UI.WX as Wx
import qualified Graphics.UI.WXCore.Image as Wx (imageGetSize)

import BigBang.Util.ImageConvert as ImageConvert

-------------------------------------------------------------------------------

{-
 - Typical users of this module will use Diagrams, but not Wx:
import Diagrams.Prelude
import BigBang
 -}

data Handlers world =
  Handlers { name :: Text.Text,
             size :: SizeSpec V2 Double,
             toDraw :: world -> Diagram B,
             onTick :: world -> Maybe world,
             onMouseClick :: world -> P2 Double -> Maybe world }
-- A `Nothing` value from a handler means stop the world.

handlers :: forall world . Handlers world
-- Default values for the handlers.
-- You can use record extension to fill in the handlers you want
-- while leaving out the ones you don't need.

---------

bigBang :: forall world . world -> Handlers world -> IO ()

{- Examples:
> :set +m
> (bigBang
    1.0
    handlers { BigBang.size = dims2D 400 400,
               toDraw = \x -> square (1.0 / x) `atopBG` square 1 # fc white,
               onTick = \x -> Just (x + 0.1),
               onMouseClick = \x _ -> Just (x * 0.5) })

> (bigBang
    []
    handlers { BigBang.size = dims2D 400 400,
               toDraw = \pts -> mconcat [moveTo p (circle 0.01 # fc black) | p <- pts] `atopBG` square 1 # fc white,
               onMouseClick = \pts pt -> Just (pt : pts) })
-}
bigBang start
         handlers
  =
  Wx.start
   (do -- create a non-user-resizable top-level frame
       f <- frameFixed [Wx.text := Text.unpack (name handlers)]

       p <- bigBangPanel start handlers f

       set f [layout := widget p])

---------

atopBG x y =
  (x `atop` y) # withEnvelope y

-------------------------------------------------------------------------------

bigBangPanel :: forall world . world -> Handlers world -> Wx.Frame () -> IO (Wx.Panel ())
bigBangPanel start
             Handlers { name = _,
                        size = size_spec,
                        toDraw = draw,
                        onTick = tick,
                        onMouseClick = handle_click }
             f
  =
  do -- draw the start diagram and get the start coordinates
     (startImg, startCoords) <- diagramToWx size_spec (draw start)
     Wx.Size startW startH   <- Wx.imageGetSize startImg

     -- variable that will hold the current state of the world
     currentWorld  <- varCreate start

     -- variable that will hold the way to transform coordinates from Wx to Diagram space
     currentCoords <- varCreate startCoords

     -- create a panel to draw in.
     p             <- panel f [layout   := space startW startH,
                               on paint := paint_world (currentWorld, currentCoords)]

     -- create a timer that updates the world on every tick
     t             <- timer f [interval := 20]
     set t [on command := next_world (currentWorld, p, t)]

     -- react on user input
     set p [on click := handle_wx_click (currentWorld, currentCoords, p, t)]

     return p

   where
     handleResult :: (Var world, Wx.Panel (), Wx.Timer) -> Maybe world -> IO ()
     handleResult (currentWorld, p, t) result =
       case result of
         Nothing   -> do set t [enabled := False]
                         error "bad"
         Just next -> do varSet currentWorld next
                         repaint p

     paint_world :: (Var world, Var (Wx.Point -> P2 Double)) -> Wx.DC () -> Wx.Rect -> IO ()
     paint_world (currentWorld, currentCoords) dc rect =
       let Wx.Size w h = Wx.rectSize rect
       in
         do world         <- varGet currentWorld
            (img, coords) <- diagramToWx (Dia.dims2D (fromIntegral w) (fromIntegral h)) (draw world)
            Wx.drawImage dc img (Wx.rectTopLeft rect) []
            varSet currentCoords coords

     next_world :: (Var world, Wx.Panel (), Wx.Timer) -> IO ()
     next_world (currentWorld, p, t) =
       do world <- varGet currentWorld
          handleResult (currentWorld, p, t) (tick world)

     handle_wx_click (currentWorld, currentCoords, p, t) pt =
       do world  <- varGet currentWorld
          coords <- varGet currentCoords
          handleResult (currentWorld, p, t) (handle_click world (coords pt))

-------------------------------------------------------------------------------

-- Default values for the handlers (see type declaration above).
handlers = Handlers { name = "World",
                      size = absolute,
                      toDraw = \w -> mempty,
                      onTick = \w -> Just w,
                      onMouseClick = \w p -> Just w }

-------------------------------------------------------------------------------