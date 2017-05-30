{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.TumblrPostSpec (main, spec) where

import Test.Hspec
import Text.TumblrPost
import Text.TumblrPost.Internal (TopLevel(..))
import Text.TumblrPost.InternalSpec hiding (main, spec)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as BL
import Data.String.Here.Uninterpolated (hereFile)

answer :: TopLevel
answer = fromJust (decode answerJSON)

audio :: TopLevel
audio = fromJust (decode audioJSON)

chat :: TopLevel
chat = fromJust (decode chatJSON)

link :: TopLevel
link = fromJust (decode linkJSON)

photo :: TopLevel
photo = fromJust (decode photoJSON)

quote :: TopLevel
quote = fromJust (decode quoteJSON)

text :: TopLevel
text = fromJust (decode textJSON)

video :: TopLevel
video = fromJust (decode videoJSON)

fromUTC :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
fromUTC year month day hours minutes seconds =
  UTCTime day' (secondsToDiffTime . fromIntegral $ seconds')
  where
    day'     = fromGregorian year month day
    seconds' = 3600 * hours + 60 * minutes + seconds

answerPost :: TumblrPost
answerPost = TumblrPost {_postUrl = "http://polychora.tumblr.com/post/100643515987/i-think-game-strong-is-aave-so-dont"
                        , _content = [hereFile|test/examples/texts/answer.txt|]
                        , _date = fromUTC 2014 10 22 04 11 30
                        , _id = 100643515987}

audioPost :: TumblrPost
audioPost = TumblrPost { _postUrl = "http://polychora.tumblr.com/post/136678974792/disneygraded-boadicea-by-enya"
                       , _content = [hereFile|test/examples/texts/audio.txt|]
                       , _date = fromUTC 2016 01 05 14 42 29
                       , _id = 136678974792}

chatPost :: TumblrPost
chatPost = TumblrPost {_postUrl = "http://polychora.tumblr.com/post/132627508302/friend-shows-me-a-bad-picture-they-took-of"
                      , _content = [hereFile|test/examples/texts/chat.txt|]
                      , _date = fromUTC 2015 11 05 23 06 05
                      , _id = 132627508302}

linkPost :: TumblrPost
linkPost = TumblrPost {_postUrl = "http://polychora.tumblr.com/post/100522740692/jobhaver-angelboyangelboy-facebook-deleted"
                      , _content = [hereFile|test/examples/texts/link.txt|]
                      , _date = fromUTC 2014 10 20 20 04 06
                      , _id = 100522740692}

photoPost :: TumblrPost
photoPost = TumblrPost {_postUrl = "http://polychora.tumblr.com/post/100083735852/briannamccarthy-a-song-to-my-earth-and-sky"
                       , _content = [hereFile|test/examples/texts/photo.txt|]
                       , _date = fromUTC 2014 10 15 16 08 58
                       , _id = 100083735852}

quotePost :: TumblrPost
quotePost = TumblrPost {_postUrl = "http://polychora.tumblr.com/post/101094725562/welcoming-people-into-the-trans-community-who"
                       , _content = [hereFile|test/examples/texts/quote.txt|]
                       , _date = fromUTC 2014 10 27 16 28 07
                       , _id = 101094725562}

textPost :: TumblrPost
textPost = TumblrPost {_postUrl = "http://polychora.tumblr.com/post/101094758347/infinitybiscuit-tbh-im-still-not-a-fan-of-even"
                      , _content = [hereFile|test/examples/texts/text.txt|]
                      , _date = fromUTC 2014 10 27 16 28 40
                      , _id = 101094758347}

videoPost :: TumblrPost
videoPost = TumblrPost {_postUrl = "http://polychora.tumblr.com/post/102611761812/blckgrlothrwrld-the-watermelon-woman-1996"
                       , _content = [hereFile|test/examples/texts/video.txt|]
                       , _date = fromUTC 2014 11 14 14 53 20
                       , _id = 102611761812}

main :: IO ()
main = hspec spec

convertsTo :: TopLevel -> TumblrPost -> Spec
convertsTo tl post = it ("converts correctly") $
  toSimplePost tl `shouldBe` Right post

spec :: Spec
spec = do
  describe "TumblrPost" $ do
    it "has various fields" $
      pending
  describe "toSimplePost" $ do
    describe "Answer" $ do
      answer `convertsTo` answerPost
    describe "Audio" $ do
      audio `convertsTo` audioPost
    describe "Chat" $ do
      chat `convertsTo` chatPost
    describe "Link" $ do
      link `convertsTo` linkPost
    describe "Photo" $ do
      photo `convertsTo` photoPost
    describe "Quote" $ do
      quote `convertsTo` quotePost
    describe "Text" $ do
      text `convertsTo` textPost
    describe "Video" $ do
      video `convertsTo` videoPost

