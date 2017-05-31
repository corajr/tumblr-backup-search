{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.TumblrPost where

import Control.Monad ((>=>), forM_)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Data.Time.Clock (UTCTime)
import Data.Aeson (decode, encode, decodeStrict')
import Data.Either (rights)
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Text.TumblrPost.Internal
import System.Environment (getArgs)
import System.Process
import System.Directory (listDirectory)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Aeson.AutoType.Alternative (alt)

getDirOrExit :: IO FilePath
getDirOrExit = do
  args <- getArgs
  case args of
    [dir] -> return dir
    _ -> error $ "USAGE: indexer dir/with/json > index.json"

data TumblrPost = TumblrPost
  { _postUrl :: Text
  , _body :: Text
  , _date :: UTCTime
  , _id :: Integer
  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 1} ''TumblrPost)

parseDate :: Text -> UTCTime
parseDate = parseTimeOrError True defaultTimeLocale fmt . T.unpack
  where
    fmt = "%Y-%m-%d %H:%M:%S GMT"

getTrailFields :: TrailElt -> Text
getTrailFields TrailElt{..} = T.intercalate " " $
  [ trailEltContent
  , blogName trailEltBlog
  ]

getDialogueFields :: DialogueElt -> Text
getDialogueFields DialogueElt{..} = T.intercalate " " $
  [ dialogueEltName
  , dialogueEltPhrase
  ]

textOrEmpty = alt id (const "")

maybeTextOrEmpty = maybe "" textOrEmpty

getTextFields :: TopLevel -> Text
getTextFields TopLevel{..} = T.intercalate " " $
  [ topLevelSummary
  , trail
  ]
  ++ map textOrEmpty topLevelTags
  ++ maybe [] (map getDialogueFields) topLevelDialogue
  ++ map maybeTextOrEmpty
  [ topLevelQuestion
  , topLevelAnswer
  , topLevelTitle
  , topLevelAlbum
  , topLevelTrackName
  , topLevelSource
  , topLevelCaption
  , topLevelText
  , topLevelLinkUrl
  , topLevelRebloggedFromUrl
  ]
  where
    trail = maybe "" (T.intercalate " " . map (alt getTrailFields (const ""))) $ topLevelTrail

toSimplePost :: TopLevel -> Either String TumblrPost
toSimplePost tl@(TopLevel{..}) = Right $ post { _body = getTextFields tl}
  where
    post = TumblrPost { _postUrl = topLevelPostUrl
                      , _date = parseDate topLevelDate
                      , _body = ""
                      , _id = topLevelId
                      }

cliMain :: IO ()
cliMain = buildIndex
-- cliMain = extractTexts

buildIndex :: IO ()
buildIndex = do
  dir <- getDirOrExit
  allFiles <- listDirectory dir
  let files = map ((dir ++ "/") ++) . filter (".json" `isSuffixOf`) $ allFiles
  (parsed :: [Maybe TopLevel]) <- mapM (BS.readFile >=> (return . decodeStrict')) files
  let processed = rights . map toSimplePost . catMaybes $ parsed
  (Just hin, _, _, _) <- createProcess (shell "node --max-old-space-size=4096 build-index.js") { std_in = CreatePipe }
  BL.hPut hin (encode processed)

extractTexts :: IO ()
extractTexts = do
  dir <- getDirOrExit
  allFiles <- listDirectory dir
  let fnames = filter (".json" `isSuffixOf`) $ allFiles
      outFnames = map (T.unpack . T.replace ".json" ".txt" . T.pack) fnames
      files = map ((dir ++ "/") ++) fnames
      outFiles = map ((dir ++ "/texts/") ++) outFnames
  forM_ (zip files outFiles) $ \(inFile, outFile) -> do
    (Just x :: Maybe TopLevel) <- BL.readFile inFile >>= return . decode
    T.writeFile outFile (getTextFields x)
