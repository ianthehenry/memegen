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
import           Data.Text (unpack, strip, toUpper, Text)
import qualified Data.Text as Text
import           Data.Char (isLetter)
import           Network.Wai.Handler.Warp as Warp
import           Control.Lens
import qualified Data.Configurator as Conf

data Command = ListMemes | MakeMeme Meme | AmbiguousSide
instance Show Command where
  show ListMemes = "list"
  show (MakeMeme (Meme a b c)) = show [a, unpack b, unpack c]
  show AmbiguousSide = "ambiguous"

inputParser :: Parser Command
inputParser = parseNothing
  <|> parseComplete
  <|> parseIncomplete
  where parseNothing = endOfInput *> return ListMemes
        parseComplete = do
          templateName <- word
          topText <- takeWhile1 (/= '|')
          take 1
          bottomText <- takeWhile (return True)
          return $ MakeMeme $ Meme (unpack templateName)
                                   (toUpper $ strip topText)
                                   (toUpper $ strip bottomText)
          where word = takeWhile1 isLetter
        parseIncomplete = return AmbiguousSide

usageMessage :: Set String -> Text
usageMessage memes =
  Text.intercalate "\n" [ "Usage: /meme memename (top text) | (bottom text)"
                        , "That's a pipe character separating the top from the bottom."
                        , "See https://github.com/ianthehenry/memegen/tree/master/templates for a complete list of memes, and open a pull request to add your own. Or try to figure out what it might be from this list:"
                        , ""
                        , Text.intercalate " " (Set.toAscList (Set.map Text.pack memes))
                        ]

makeUniqueTemplate :: String -> String -> Meme -> IO TR.Message
makeUniqueTemplate localPath remotePath meme = do
  let filename = "test.png"
  renderMeme meme (localPath </> filename)
  return $ TR.defaultMessage & TR.iconEmoji .~ TR.Icon "helicopter"
                             & TR.text .~ Text.pack (remotePath </> filename)
                             & TR.username .~ "memebot"

handler :: MemeBot -> TR.Command -> TR.Slack Text
handler (MemeBot templates localPath remotePath) command =
  case parseOnly inputParser (command ^. TR.text) of
    Left _ -> return (usageMessage templates)
    Right ListMemes -> return (usageMessage templates)
    Right AmbiguousSide -> return "You gotta put the pipe somewhere!"
    Right (MakeMeme meme@(Meme templateName _ _))
      | templateName `Set.member` templates -> do
        message <- TR.liftIO $ makeUniqueTemplate localPath remotePath meme
        TR.say (message & TR.destination .~ (command ^. TR.source))
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
