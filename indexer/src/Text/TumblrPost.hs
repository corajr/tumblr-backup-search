{-# LANGUAGE TemplateHaskell #-}
module Text.TumblrPost where

import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Aeson (decode)
import Text.TumblrPost.Internal

cliMain :: IO ()
cliMain = return ()

data TumblrPost = TumblrPost
  { _postUrl :: Text
  , _blogName :: Text
  , _caption :: Text
  , _content :: Text
  , _date :: UTCTime
  , _timestamp :: UTCTime
  , _id :: Integer
  , _type :: Text
  } deriving (Eq, Show)

toSimplePost :: TopLevel -> TumblrPost
toSimplePost = undefined
