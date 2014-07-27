module Rendering (
  renderMeme,
  Meme(..)
) where
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Pango (PangoLayout, PangoRectangle(..))
import qualified Graphics.Rendering.Pango as Pango
import Data.Functor ((<$>))
import System.Directory (getDirectoryContents)
import Data.Set ((\\))
import qualified Data.Set as Set
import System.FilePath (dropExtension, (<.>), (</>))

memeFont :: Double -> IO Pango.FontDescription
memeFont size = do
  font <- Pango.fontDescriptionNew
  Pango.fontDescriptionSetFamily font "Impact"
  Pango.fontDescriptionSetSize font size
  return font

configureLayout :: PangoLayout -> Pango.FontDescription -> Double -> IO ()
configureLayout layout font width = do
  Pango.layoutSetFontDescription layout (Just font)
  Pango.layoutSetWidth layout (Just width)
  Pango.layoutSetAlignment layout Pango.AlignCenter

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) val = max (min high val) low

fontSize :: Double -> String -> Double
fontSize width text = clamp (30, 80) estimate
  where estimate = width / fromIntegral (length text)

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
      (_, PangoRectangle _ _ _ bottomHeight) <- Pango.layoutGetExtents bottomLayout
      return bottomHeight

    Cairo.moveTo 0 0
    Pango.layoutPath topLayout

    Cairo.moveTo 0 (height - bottomHeight)
    Pango.layoutPath bottomLayout

    Cairo.setSourceRGBA 0 0 0 1
    Cairo.setLineWidth 4
    Cairo.setLineJoin Cairo.LineJoinBevel
    Cairo.strokePreserve
    Cairo.setSourceRGBA 1 1 1 1
    Cairo.fill

data Meme = Meme String String String

renderMeme :: Meme -> FilePath -> IO ()
renderMeme (Meme templateName topText bottomText) outPath =
  Cairo.withImageSurfaceFromPNG path $ \surface ->
    memeText surface topText bottomText >>
    Cairo.surfaceWriteToPNG surface outPath
  where path = "templates" </> templateName <.> "png"
