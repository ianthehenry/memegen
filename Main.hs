{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Rendering
import           System.Directory (getDirectoryContents)
import           Data.Set ((\\), Set)
import qualified Data.Set as Set
import           System.FilePath (dropExtension, (<.>), (</>))
import qualified Network.Tightrope as TR
import           Data.Attoparsec.Text
import           Control.Applicative
import           Prelude hiding (take, takeWhile)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Char (isSpace)
import           Network.Wai.Handler.Warp as Warp
import           Control.Lens ((^.))
import           Data.Monoid (mconcat)
import qualified Data.Configurator as Conf
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID

data Command = ListMemes | MakeMeme Meme | AmbiguousSide
instance Show Command where
  show ListMemes = "list"
  show (MakeMeme (Meme a b c)) = show [a, Text.unpack b, Text.unpack c]
  show AmbiguousSide = "ambiguous"

inputParser :: Parser Command
inputParser = parseNothing
  <|> parseComplete
  <|> parseNoText
  <|> parseIncomplete
  where parseNothing = endOfInput *> return ListMemes
        parseNoText = (\t -> MakeMeme (Meme t "" "")) <$> parseTemplate <* endOfInput
        parseTemplate = Text.unpack . Text.toLower <$> takeTill isSpace
        parseComplete = do
          templateName <- parseTemplate
          topText <- takeTill (== '|')
          take 1
          bottomText <- takeText
          return $ MakeMeme $ Meme templateName
                                   (Text.toUpper $ Text.strip topText)
                                   (Text.toUpper $ Text.strip bottomText)
        parseIncomplete = return AmbiguousSide

usageMessage :: Set String -> Text
usageMessage memes =
  Text.intercalate "\n" [ "Usage: /meme memename (top text) | (bottom text)"
                        , "That's a pipe character separating the top from the bottom."
                        , "See https://github.com/ianthehenry/memegen/tree/master/templates for a complete list of memes, and open a pull request to add your own. Or try to figure out what it might be from this list:"
                        , ""
                        , Text.intercalate " " (Set.toAscList (Set.map Text.pack memes))
                        ]

saveMeme :: String -> Meme -> IO FilePath
saveMeme localPath meme = do
  filename <- (<.> "png") . UUID.toString <$> UUID.nextRandom
  renderMeme meme (localPath </> filename)
  return filename
  
handler :: MemeBot -> TR.Command -> TR.Slack Text
handler (MemeBot templates localPath remotePath) command =
  case parseOnly inputParser (Text.strip $ command ^. TR.text) of
    Left _ -> return (usageMessage templates)
    Right ListMemes -> return (usageMessage templates)
    Right AmbiguousSide -> return "You gotta put the pipe somewhere!"
    Right (MakeMeme meme@(Meme templateName _ _))
      | templateName `Set.member` templates -> do
        filename <- TR.liftIO $ saveMeme localPath meme

        let messageText = mconcat ["<"
                                  , Text.pack (remotePath </> filename)
                                  , "|"
                                  , "MEME"
                                  , ">"
                                  ]
        TR.say $ TR.message (TR.Icon "helicopter")
                            "memebot"
                            messageText
                            (command ^. TR.source)
        return ""
      | otherwise -> return $ Text.append "Unknown template " (Text.pack templateName)

data MemeBot = MemeBot (Set String) String String

main :: IO ()
main = do
  conf <- Conf.load [Conf.Required "conf"]
  files <- getDirectoryContents "templates/"
  [token, localPath, remotePath, hookPath] <- sequence $
    Conf.require conf <$> [ "incoming-token"
                          , "local-path"
                          , "remote-path"
                          , "incoming-hook"
                          ]
  port <- Conf.require conf "port"
  let fileNames = Set.fromList files \\ Set.fromList [".", ".."]
      templates = Set.map dropExtension fileNames
      memebot = MemeBot templates localPath remotePath
      bot = TR.bot (TR.Account token hookPath) (handler memebot)

  putStrLn $ "Running on port " ++ show port
  Warp.run port bot
