module Main where
import Rendering
import System.Directory (getDirectoryContents)
import Data.Set ((\\))
import qualified Data.Set as Set
import System.FilePath (dropExtension, (<.>), (</>))

main :: IO ()
main = do
  let name = "success-kid"
  files <- getDirectoryContents "templates/"
  let fileNames = Set.fromList files \\ Set.fromList [".", ".."]
  let templates = Set.map dropExtension fileNames
  if Set.member name templates then
    renderMeme (Meme name "BUILT A MEME GENERATOR" "IN HASKELL") "test.png" >>
    putStrLn "okay"
  else
    putStrLn "I don't have that template"
