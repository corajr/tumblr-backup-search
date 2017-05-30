{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Text.TumblrPost.Internal.Quote where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import           GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data Reblog = Reblog { 
    reblogTreeHtml :: Text,
    reblogComment :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Reblog where
  parseJSON (Object v) = Reblog <$> v .:   "tree_html" <*> v .:   "comment"
  parseJSON _          = mzero


instance ToJSON Reblog where
  toJSON     (Reblog {..}) = object ["tree_html" .= reblogTreeHtml, "comment" .= reblogComment]
  toEncoding (Reblog {..}) = pairs  ("tree_html" .= reblogTreeHtml<>"comment" .= reblogComment)


data TopLevel = TopLevel { 
    topLevelPostUrl :: Text,
    topLevelSummary :: Text,
    topLevelNoteCount :: Double,
    topLevelRebloggedFromId :: Text,
    topLevelDisplayAvatar :: Bool,
    topLevelRebloggedRootId :: Text,
    topLevelRecommendedSource :: (Maybe Value),
    topLevelState :: Text,
    topLevelRebloggedRootUrl :: Text,
    topLevelRebloggedFromUrl :: Text,
    topLevelSourceTitle :: Text,
    topLevelSlug :: Text,
    topLevelText :: Text,
    topLevelFormat :: Text,
    topLevelBlogName :: Text,
    topLevelShortUrl :: Text,
    topLevelRebloggedFromCanMessage :: Bool,
    topLevelRebloggedRootCanMessage :: Bool,
    topLevelReblogKey :: Text,
    topLevelDate :: Text,
    topLevelRebloggedFromUuid :: Text,
    topLevelRebloggedRootUuid :: Text,
    topLevelRecommendedColor :: (Maybe Value),
    topLevelCanReblog :: Bool,
    topLevelSource :: Text,
    topLevelId :: Double,
    topLevelCanLike :: Bool,
    topLevelType :: Text,
    topLevelCanSendInMessage :: Bool,
    topLevelTimestamp :: Double,
    topLevelRebloggedRootName :: Text,
    topLevelRebloggedFromName :: Text,
    topLevelCanReply :: Bool,
    topLevelRebloggedRootTitle :: Text,
    topLevelRebloggedFromTitle :: Text,
    topLevelSourceUrl :: Text,
    topLevelTags :: [(Maybe Value)],
    topLevelReblog :: Reblog
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "post_url" <*> v .:   "summary" <*> v .:   "note_count" <*> v .:   "reblogged_from_id" <*> v .:   "display_avatar" <*> v .:   "reblogged_root_id" <*> v .:?? "recommended_source" <*> v .:   "state" <*> v .:   "reblogged_root_url" <*> v .:   "reblogged_from_url" <*> v .:   "source_title" <*> v .:   "slug" <*> v .:   "text" <*> v .:   "format" <*> v .:   "blog_name" <*> v .:   "short_url" <*> v .:   "reblogged_from_can_message" <*> v .:   "reblogged_root_can_message" <*> v .:   "reblog_key" <*> v .:   "date" <*> v .:   "reblogged_from_uuid" <*> v .:   "reblogged_root_uuid" <*> v .:?? "recommended_color" <*> v .:   "can_reblog" <*> v .:   "source" <*> v .:   "id" <*> v .:   "can_like" <*> v .:   "type" <*> v .:   "can_send_in_message" <*> v .:   "timestamp" <*> v .:   "reblogged_root_name" <*> v .:   "reblogged_from_name" <*> v .:   "can_reply" <*> v .:   "reblogged_root_title" <*> v .:   "reblogged_from_title" <*> v .:   "source_url" <*> v .:   "tags" <*> v .:   "reblog"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["post_url" .= topLevelPostUrl, "summary" .= topLevelSummary, "note_count" .= topLevelNoteCount, "reblogged_from_id" .= topLevelRebloggedFromId, "display_avatar" .= topLevelDisplayAvatar, "reblogged_root_id" .= topLevelRebloggedRootId, "recommended_source" .= topLevelRecommendedSource, "state" .= topLevelState, "reblogged_root_url" .= topLevelRebloggedRootUrl, "reblogged_from_url" .= topLevelRebloggedFromUrl, "source_title" .= topLevelSourceTitle, "slug" .= topLevelSlug, "text" .= topLevelText, "format" .= topLevelFormat, "blog_name" .= topLevelBlogName, "short_url" .= topLevelShortUrl, "reblogged_from_can_message" .= topLevelRebloggedFromCanMessage, "reblogged_root_can_message" .= topLevelRebloggedRootCanMessage, "reblog_key" .= topLevelReblogKey, "date" .= topLevelDate, "reblogged_from_uuid" .= topLevelRebloggedFromUuid, "reblogged_root_uuid" .= topLevelRebloggedRootUuid, "recommended_color" .= topLevelRecommendedColor, "can_reblog" .= topLevelCanReblog, "source" .= topLevelSource, "id" .= topLevelId, "can_like" .= topLevelCanLike, "type" .= topLevelType, "can_send_in_message" .= topLevelCanSendInMessage, "timestamp" .= topLevelTimestamp, "reblogged_root_name" .= topLevelRebloggedRootName, "reblogged_from_name" .= topLevelRebloggedFromName, "can_reply" .= topLevelCanReply, "reblogged_root_title" .= topLevelRebloggedRootTitle, "reblogged_from_title" .= topLevelRebloggedFromTitle, "source_url" .= topLevelSourceUrl, "tags" .= topLevelTags, "reblog" .= topLevelReblog]
  toEncoding (TopLevel {..}) = pairs  ("post_url" .= topLevelPostUrl<>"summary" .= topLevelSummary<>"note_count" .= topLevelNoteCount<>"reblogged_from_id" .= topLevelRebloggedFromId<>"display_avatar" .= topLevelDisplayAvatar<>"reblogged_root_id" .= topLevelRebloggedRootId<>"recommended_source" .= topLevelRecommendedSource<>"state" .= topLevelState<>"reblogged_root_url" .= topLevelRebloggedRootUrl<>"reblogged_from_url" .= topLevelRebloggedFromUrl<>"source_title" .= topLevelSourceTitle<>"slug" .= topLevelSlug<>"text" .= topLevelText<>"format" .= topLevelFormat<>"blog_name" .= topLevelBlogName<>"short_url" .= topLevelShortUrl<>"reblogged_from_can_message" .= topLevelRebloggedFromCanMessage<>"reblogged_root_can_message" .= topLevelRebloggedRootCanMessage<>"reblog_key" .= topLevelReblogKey<>"date" .= topLevelDate<>"reblogged_from_uuid" .= topLevelRebloggedFromUuid<>"reblogged_root_uuid" .= topLevelRebloggedRootUuid<>"recommended_color" .= topLevelRecommendedColor<>"can_reblog" .= topLevelCanReblog<>"source" .= topLevelSource<>"id" .= topLevelId<>"can_like" .= topLevelCanLike<>"type" .= topLevelType<>"can_send_in_message" .= topLevelCanSendInMessage<>"timestamp" .= topLevelTimestamp<>"reblogged_root_name" .= topLevelRebloggedRootName<>"reblogged_from_name" .= topLevelRebloggedFromName<>"can_reply" .= topLevelCanReply<>"reblogged_root_title" .= topLevelRebloggedRootTitle<>"reblogged_from_title" .= topLevelRebloggedFromTitle<>"source_url" .= topLevelSourceUrl<>"tags" .= topLevelTags<>"reblog" .= topLevelReblog)




parse :: FilePath -> IO TopLevel
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


