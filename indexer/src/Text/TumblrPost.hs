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
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Data.Time.Clock (UTCTime)
import Data.Aeson (decode, encode, decodeStrict')
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

cliMain :: IO ()
cliMain = do
  dir <- getDirOrExit
  allFiles <- listDirectory dir
  let files = map ((dir ++ "/") ++) . filter (".json" `isSuffixOf`) $ allFiles
  (parsed :: [Maybe TopLevel]) <- mapM (BS.readFile >=> (return . decodeStrict')) files
  (Just hin, _, _, _) <- createProcess (shell "node build-index.js") { std_in = CreatePipe }
  BL.hPut hin (encode (catMaybes parsed))

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

toSimplePost :: TopLevel -> Either String TumblrPost
toSimplePost tl@(TopLevel{..}) = Right $ post { _body = T.pack . BL.unpack . encode $ tl }
  where
    post = TumblrPost { _postUrl = topLevelPostUrl
                      , _date = parseDate topLevelDate
                      , _body = ""
                      , _id = topLevelId
                      }
