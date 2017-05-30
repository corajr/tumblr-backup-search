{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.TumblrPostSpec (main, spec) where

import Test.Hspec
import Text.TumblrPost
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Data.String.Here.Uninterpolated (hereFile)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TumblrPost" $ do
    it "has various fields" $
      pending
