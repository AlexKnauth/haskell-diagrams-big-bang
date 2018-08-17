module BigBang.Util.ImageConvert
  (diagramToWx)
  where

import qualified Control.Lens.Fold as Fold (toListOf)

import qualified Codec.Picture as CodecPict
import qualified Codec.Picture.Types as CodecPict
import qualified Diagrams.Prelude as Dia
import qualified Diagrams.Core.Transform as Dia
import qualified Diagrams.Backend.Rasterific as Dia
import qualified Graphics.UI.WX as Wx
import qualified Graphics.UI.WXCore as Wx (Image)


diagramToWx :: (Dia.Monoid' q) =>
               Dia.SizeSpec Dia.V2 Double
               -> Dia.QDiagram Dia.Rasterific Dia.V2 Double q
               -> IO (Wx.Image (), Wx.Point -> Dia.P2 Double)
-- Example values for the `sz` argument:
--  - `mkWidth 250`
--  - `dims2D 300 400`
diagramToWx sz dia =
  let (transDiaToScreen, img) = Dia.renderDiaT Dia.Rasterific
                                               (Dia.RasterificOptions sz)
                                               dia
      h = CodecPict.imageHeight img
      transScreenToDia = Dia.inv transDiaToScreen
      screenToDia (Wx.Point sX sY) =
        Dia.papply transScreenToDia
                   (Dia.p2 (fromIntegral sX, fromIntegral (h - sY)))

  in do img <- codecPictureToWx img
        return (img, screenToDia)

codecPictureToWx :: CodecPict.Image CodecPict.PixelRGBA8 -> IO (Wx.Image ())
codecPictureToWx img =
  let w = CodecPict.imageWidth img
      h = CodecPict.imageHeight img
      d :: [CodecPict.PixelRGBA8]
      d = Fold.toListOf CodecPict.imagePixels img
  in
    Wx.imageCreateFromPixels
      (Wx.sz w h)
      (map (codecPixelToWx . CodecPict.dropTransparency) d)

codecPixelToWx :: CodecPict.PixelRGB8 -> Wx.Color
codecPixelToWx (CodecPict.PixelRGB8 r g b) =
  Wx.rgb r g b
