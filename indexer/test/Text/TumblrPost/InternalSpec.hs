{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.TumblrPost.InternalSpec (main, spec) where

import Test.Hspec
import Text.TumblrPost.Internal hiding (main)
import Data.Aeson (eitherDecode)
import Data.Either (isRight)
import qualified Data.ByteString.Lazy as BL
import Data.String.Here.Uninterpolated (hereFile)

answerJSON :: BL.ByteString
answerJSON = [hereFile|test/examples/answer.json|]

audioJSON :: BL.ByteString
audioJSON = [hereFile|test/examples/audio.json|]

chatJSON :: BL.ByteString
chatJSON = [hereFile|test/examples/chat.json|]

linkJSON :: BL.ByteString
linkJSON = [hereFile|test/examples/link.json|]

photoJSON :: BL.ByteString
photoJSON = [hereFile|test/examples/photo.json|]

quoteJSON :: BL.ByteString
quoteJSON = [hereFile|test/examples/quote.json|]

textJSON :: BL.ByteString
textJSON = [hereFile|test/examples/text.json|]

videoJSON :: BL.ByteString
videoJSON = [hereFile|test/examples/video.json|]

canParse :: BL.ByteString -> Spec
canParse json = it "can be parsed from JSON" $
  (eitherDecode json :: Either String TopLevel) `shouldSatisfy` isRight

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TopLevel" $ do
    describe "Answer" $ do
      canParse answerJSON
    describe "Audio" $ do
      canParse audioJSON
    describe "Chat" $ do
      canParse chatJSON
    describe "Link" $ do
      canParse linkJSON
    describe "Photo" $ do
      canParse photoJSON
    describe "Quote" $ do
      canParse quoteJSON
    describe "Text" $ do
      canParse textJSON
    describe "Video" $ do
      canParse videoJSON
