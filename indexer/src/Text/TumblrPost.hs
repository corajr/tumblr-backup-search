{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.TumblrPost where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Data.Time.Clock (UTCTime)
import Data.Aeson (decode)
import Text.TumblrPost.Internal

cliMain :: IO ()
cliMain = return ()

data TumblrPost = TumblrPost
  { _postUrl :: Text
  , _content :: Text
  , _date :: UTCTime
  , _id :: Integer
  } deriving (Eq, Show)


parseDate :: Text -> UTCTime
parseDate = parseTimeOrError True defaultTimeLocale fmt . T.unpack
  where
    fmt = "%Y-%m-%d %H:%M:%S GMT"

toSimplePost :: TopLevel -> Either String TumblrPost
toSimplePost (TopLevel{..}) =
  case topLevelType of
    "answer" -> Right post
    "audio" -> Right post
    "chat" -> Right post
    "link" -> Right post
    "photo" -> Right post
    "quote" -> Right post
    "text" -> Right post
    "video" -> Right post
    _ -> Left "unknown type"
  where
    post = TumblrPost { _postUrl = topLevelPostUrl
                      , _date = parseDate topLevelDate
                      , _content = ""
                      , _id = topLevelId
                      }
