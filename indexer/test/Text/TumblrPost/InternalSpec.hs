{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.TumblrPost.InternalSpec (main, spec) where

import Test.Hspec
import Text.TumblrPost.Internal
import Data.Aeson (eitherDecode)
import Data.Either (isRight)
import qualified Data.ByteString.Lazy as BL
import Data.String.Here.Uninterpolated (hereFile)

photoJSON :: BL.ByteString
photoJSON = [hereFile|test/examples/photo.json|]

canParse :: BL.ByteString -> Spec
canParse json = it "can be parsed from JSON" $
  (eitherDecode json :: Either String TopLevel) `shouldSatisfy` isRight

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TopLevel" $
    describe "Photo" $
      canParse photoJSON
