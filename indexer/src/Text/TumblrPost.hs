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
import Text.Pandoc
import Data.Aeson.AutoType.Alternative (alt)

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

houseStyle :: Pandoc -> Text
houseStyle = T.pack . writeMarkdown def

parseHTML :: Text -> Text
parseHTML =
  houseStyle
  . handleError
  . readHtml def
  . T.unpack

answerContent :: TopLevel -> Text
answerContent (TopLevel{..}) = parseHTML ans
  where
    ans = maybe "" (alt id (const "")) topLevelAnswer

audioContent :: TopLevel -> Text
audioContent (TopLevel{..}) = undefined

chatContent :: TopLevel -> Text
chatContent (TopLevel{..}) = undefined

linkContent :: TopLevel -> Text
linkContent (TopLevel{..}) = undefined

photoContent :: TopLevel -> Text
photoContent (TopLevel{..}) = undefined

quoteContent :: TopLevel -> Text
quoteContent (TopLevel{..}) = undefined

textContent :: TopLevel -> Text
textContent (TopLevel{..}) = undefined

videoContent :: TopLevel -> Text
videoContent (TopLevel{..}) = undefined

toSimplePost :: TopLevel -> Either String TumblrPost
toSimplePost tl@(TopLevel{..}) =
  case topLevelType of
    "answer" -> withExtractor answerContent
    "audio" -> withExtractor audioContent
    "chat" -> withExtractor chatContent
    "link" -> withExtractor linkContent
    "photo" -> withExtractor photoContent
    "quote" -> withExtractor quoteContent
    "text" -> withExtractor textContent
    "video" -> withExtractor videoContent
    _ -> Left "unknown type"
  where
    withExtractor f = Right (post { _content = f tl })
    post = TumblrPost { _postUrl = topLevelPostUrl
                      , _date = parseDate topLevelDate
                      , _content = ""
                      , _id = topLevelId
                      }
