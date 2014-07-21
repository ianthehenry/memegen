module Main where
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango
import Data.Functor ((<$>))

data Color = Color Double Double Double Double

setColor :: Color -> Cairo.Render ()
setColor (Color r g b a) = Cairo.setSourceRGBA r g b a

bg :: Color -> Cairo.Render ()
bg color = Cairo.withTargetSurface $ \surface -> do
  setColor color
  width <- Cairo.imageSurfaceGetWidth surface
  height <- Cairo.imageSurfaceGetHeight surface
  Cairo.rectangle 0 0 (fromIntegral width) (fromIntegral height)
  Cairo.fill

memeFont :: Double -> IO Pango.FontDescription
memeFont size = do
  font <- Pango.fontDescriptionNew
  Pango.fontDescriptionSetFamily font "Impact"
  Pango.fontDescriptionSetSize font size
  return font

configureLayout :: Pango.PangoLayout -> Pango.FontDescription -> Double -> IO ()
configureLayout layout font width = do
  Pango.layoutSetFontDescription layout (Just font)
  Pango.layoutSetWidth layout (Just width)
  Pango.layoutSetAlignment layout Pango.AlignCenter

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) val = max (min high val) low

fontSize :: Double -> String -> Double
fontSize width text = clamp (30, 80) estimate
  where estimate = width / (fromIntegral $ length text)

memeText :: Cairo.Surface -> String -> String -> IO ()
memeText surface topText bottomText = do
  width <- fromIntegral <$> Cairo.imageSurfaceGetWidth surface
  height <- fromIntegral <$> Cairo.imageSurfaceGetHeight surface

  topFont <- memeFont (fontSize width topText)
  bottomFont <- memeFont (fontSize width bottomText)

  Cairo.renderWith surface $ do
    topLayout <- Pango.createLayout topText
    bottomLayout <- Pango.createLayout bottomText

    bottomHeight <- Cairo.liftIO $ do
      configureLayout topLayout topFont width
      configureLayout bottomLayout bottomFont width
      (_, (Pango.PangoRectangle _ _ _ bottomHeight)) <- Pango.layoutGetExtents bottomLayout
      return bottomHeight

    Cairo.moveTo 0 0
    Pango.layoutPath topLayout

    Cairo.moveTo 0 (height - bottomHeight)
    Pango.layoutPath bottomLayout

    setColor (Color 0 0 0 1)
    Cairo.setLineWidth 4
    Cairo.setLineJoin Cairo.LineJoinBevel
    Cairo.strokePreserve
    setColor (Color 1 1 1 1)
    Cairo.fill

    return ()

fillAndSave :: Cairo.Surface -> IO ()
fillAndSave surface = do
  memeText surface "BUILT A MEME GENERATOR" "IN HASKELL"
  Cairo.surfaceWriteToPNG surface "test.png"

main :: IO ()
main =
  -- Cairo.withImageSurface Cairo.FormatARGB32 480 480 fillAndSave
  Cairo.withImageSurfaceFromPNG "success.png" fillAndSave

