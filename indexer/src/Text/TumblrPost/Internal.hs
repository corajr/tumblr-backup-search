{-# LANGUAGE TemplateHaskell #-}
module Text.TumblrPost.Internal where

import Data.Aeson.TH

import Data.Char (toLower)
import qualified Text.TumblrPost.Internal.Answer as Answer
import qualified Text.TumblrPost.Internal.Chat as Chat
import qualified Text.TumblrPost.Internal.Link as Link
import qualified Text.TumblrPost.Internal.Photo as Photo
import qualified Text.TumblrPost.Internal.Quote as Quote
import qualified Text.TumblrPost.Internal.Text as Text
import qualified Text.TumblrPost.Internal.Video as Video

data TopLevel = Answer Answer.TopLevel
              | Chat Chat.TopLevel
              | Link Link.TopLevel
              | Photo Photo.TopLevel
              | Quote Quote.TopLevel
              | Text Text.TopLevel
              | Video Video.TopLevel
              deriving (Eq, Show)

$(deriveJSON defaultOptions{constructorTagModifier = map toLower, sumEncoding = defaultTaggedObject{tagFieldName = "type"}} ''TopLevel)
