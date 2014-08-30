module Rendering (
  renderMeme,
  Meme(..)
) where
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.Rendering.Pango (PangoLayout, PangoRectangle(..))
import qualified Graphics.Rendering.Pango as Pango
import           Data.Text (Text, length, unpack)
import           Data.Functor ((<$>))
import           System.FilePath ((<.>), (</>))
import           Prelude hiding (length)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.Random (getStdRandom, randomR)
import           Data.List ((\\))
import qualified Data.List as List

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

fontSize :: Double -> Text -> Double
fontSize width text = clamp (30, 80) estimate
  where estimate = width / fromIntegral (length text)

memeText :: Cairo.Surface -> Text -> Text -> IO ()
memeText surface topText bottomText = do
  width <- fromIntegral <$> Cairo.imageSurfaceGetWidth surface
  height <- fromIntegral <$> Cairo.imageSurfaceGetHeight surface

  topFont <- memeFont (fontSize width topText)
  bottomFont <- memeFont (fontSize width bottomText)

  Cairo.renderWith surface $ do
    topLayout <- Pango.createLayout (unpack topText)
    bottomLayout <- Pango.createLayout (unpack bottomText)

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

data Meme = Meme String Text Text

randomFile :: [FilePath] -> IO FilePath
randomFile contents = (files !!) <$> getStdRandom (randomR (0, List.length files - 1))
  where files = contents \\ [".", ".."]

filePathForTemplate :: String -> IO FilePath
filePathForTemplate name = do
  let path = "templates" </> name
  isDirectory <- doesDirectoryExist path
  if isDirectory then
    (path </>) <$> (randomFile =<< getDirectoryContents path)
  else return (path <.> "png")

renderMeme :: Meme -> FilePath -> IO ()
renderMeme (Meme templateName topText bottomText) outPath = do
  path <- filePathForTemplate templateName
  Cairo.withImageSurfaceFromPNG path $ \surface ->
    memeText surface topText bottomText >>
    Cairo.surfaceWriteToPNG surface outPath
