{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Text.TumblrPost.Internal.Link where

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


data Post = Post { 
    postId :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Post where
  parseJSON (Object v) = Post <$> v .:   "id"
  parseJSON _          = mzero


instance ToJSON Post where
  toJSON     (Post {..}) = object ["id" .= postId]
  toEncoding (Post {..}) = pairs  ("id" .= postId)


data Theme = Theme { 
    themeHeaderFocusHeight :: Double,
    themeBodyFont :: Text,
    themeShowTitle :: Bool,
    themeHeaderImageScaled :: Text,
    themeBackgroundColor :: Text,
    themeHeaderStretch :: Bool,
    themeLinkColor :: Text,
    themeHeaderImageFocused :: Text,
    themeHeaderFocusWidth :: Double,
    themeShowAvatar :: Bool,
    themeShowHeaderImage :: Bool,
    themeTitleFontWeight :: Text,
    themeHeaderFullHeight :: Double,
    themeAvatarShape :: Text,
    themeHeaderBounds :: Text,
    themeTitleFont :: Text,
    themeHeaderImage :: Text,
    themeTitleColor :: Text,
    themeShowDescription :: Bool,
    themeHeaderFullWidth :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON Theme where
  parseJSON (Object v) = Theme <$> v .:   "header_focus_height" <*> v .:   "body_font" <*> v .:   "show_title" <*> v .:   "header_image_scaled" <*> v .:   "background_color" <*> v .:   "header_stretch" <*> v .:   "link_color" <*> v .:   "header_image_focused" <*> v .:   "header_focus_width" <*> v .:   "show_avatar" <*> v .:   "show_header_image" <*> v .:   "title_font_weight" <*> v .:   "header_full_height" <*> v .:   "avatar_shape" <*> v .:   "header_bounds" <*> v .:   "title_font" <*> v .:   "header_image" <*> v .:   "title_color" <*> v .:   "show_description" <*> v .:   "header_full_width"
  parseJSON _          = mzero


instance ToJSON Theme where
  toJSON     (Theme {..}) = object ["header_focus_height" .= themeHeaderFocusHeight, "body_font" .= themeBodyFont, "show_title" .= themeShowTitle, "header_image_scaled" .= themeHeaderImageScaled, "background_color" .= themeBackgroundColor, "header_stretch" .= themeHeaderStretch, "link_color" .= themeLinkColor, "header_image_focused" .= themeHeaderImageFocused, "header_focus_width" .= themeHeaderFocusWidth, "show_avatar" .= themeShowAvatar, "show_header_image" .= themeShowHeaderImage, "title_font_weight" .= themeTitleFontWeight, "header_full_height" .= themeHeaderFullHeight, "avatar_shape" .= themeAvatarShape, "header_bounds" .= themeHeaderBounds, "title_font" .= themeTitleFont, "header_image" .= themeHeaderImage, "title_color" .= themeTitleColor, "show_description" .= themeShowDescription, "header_full_width" .= themeHeaderFullWidth]
  toEncoding (Theme {..}) = pairs  ("header_focus_height" .= themeHeaderFocusHeight<>"body_font" .= themeBodyFont<>"show_title" .= themeShowTitle<>"header_image_scaled" .= themeHeaderImageScaled<>"background_color" .= themeBackgroundColor<>"header_stretch" .= themeHeaderStretch<>"link_color" .= themeLinkColor<>"header_image_focused" .= themeHeaderImageFocused<>"header_focus_width" .= themeHeaderFocusWidth<>"show_avatar" .= themeShowAvatar<>"show_header_image" .= themeShowHeaderImage<>"title_font_weight" .= themeTitleFontWeight<>"header_full_height" .= themeHeaderFullHeight<>"avatar_shape" .= themeAvatarShape<>"header_bounds" .= themeHeaderBounds<>"title_font" .= themeTitleFont<>"header_image" .= themeHeaderImage<>"title_color" .= themeTitleColor<>"show_description" .= themeShowDescription<>"header_full_width" .= themeHeaderFullWidth)


data Blog = Blog { 
    blogShareFollowing :: Bool,
    blogActive :: Bool,
    blogName :: Text,
    blogShareLikes :: Bool,
    blogCanBeFollowed :: Bool,
    blogTheme :: Theme
  } deriving (Show,Eq,Generic)


instance FromJSON Blog where
  parseJSON (Object v) = Blog <$> v .:   "share_following" <*> v .:   "active" <*> v .:   "name" <*> v .:   "share_likes" <*> v .:   "can_be_followed" <*> v .:   "theme"
  parseJSON _          = mzero


instance ToJSON Blog where
  toJSON     (Blog {..}) = object ["share_following" .= blogShareFollowing, "active" .= blogActive, "name" .= blogName, "share_likes" .= blogShareLikes, "can_be_followed" .= blogCanBeFollowed, "theme" .= blogTheme]
  toEncoding (Blog {..}) = pairs  ("share_following" .= blogShareFollowing<>"active" .= blogActive<>"name" .= blogName<>"share_likes" .= blogShareLikes<>"can_be_followed" .= blogCanBeFollowed<>"theme" .= blogTheme)


data TrailElt = TrailElt { 
    trailEltContentRaw :: Text,
    trailEltPost :: Post,
    trailEltBlog :: Blog,
    trailEltContent :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON TrailElt where
  parseJSON (Object v) = TrailElt <$> v .:   "content_raw" <*> v .:   "post" <*> v .:   "blog" <*> v .:   "content"
  parseJSON _          = mzero


instance ToJSON TrailElt where
  toJSON     (TrailElt {..}) = object ["content_raw" .= trailEltContentRaw, "post" .= trailEltPost, "blog" .= trailEltBlog, "content" .= trailEltContent]
  toEncoding (TrailElt {..}) = pairs  ("content_raw" .= trailEltContentRaw<>"post" .= trailEltPost<>"blog" .= trailEltBlog<>"content" .= trailEltContent)


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
    topLevelLinkAuthor :: (Maybe Value),
    topLevelRebloggedRootUrl :: Text,
    topLevelRebloggedFromUrl :: Text,
    topLevelTrail :: [TrailElt],
    topLevelSlug :: Text,
    topLevelUrl :: Text,
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
    topLevelId :: Double,
    topLevelExcerpt :: (Maybe Value),
    topLevelCanLike :: Bool,
    topLevelTitle :: (Maybe Value),
    topLevelType :: Text,
    topLevelCanSendInMessage :: Bool,
    topLevelTimestamp :: Double,
    topLevelPublisher :: Text,
    topLevelRebloggedRootName :: Text,
    topLevelRebloggedFromName :: Text,
    topLevelDescription :: Text,
    topLevelCanReply :: Bool,
    topLevelRebloggedRootTitle :: Text,
    topLevelRebloggedFromTitle :: Text,
    topLevelTags :: [(Maybe Value)],
    topLevelReblog :: Reblog
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "post_url" <*> v .:   "summary" <*> v .:   "note_count" <*> v .:   "reblogged_from_id" <*> v .:   "display_avatar" <*> v .:   "reblogged_root_id" <*> v .:?? "recommended_source" <*> v .:   "state" <*> v .:?? "link_author" <*> v .:   "reblogged_root_url" <*> v .:   "reblogged_from_url" <*> v .:   "trail" <*> v .:   "slug" <*> v .:   "url" <*> v .:   "format" <*> v .:   "blog_name" <*> v .:   "short_url" <*> v .:   "reblogged_from_can_message" <*> v .:   "reblogged_root_can_message" <*> v .:   "reblog_key" <*> v .:   "date" <*> v .:   "reblogged_from_uuid" <*> v .:   "reblogged_root_uuid" <*> v .:?? "recommended_color" <*> v .:   "can_reblog" <*> v .:   "id" <*> v .:?? "excerpt" <*> v .:   "can_like" <*> v .:?? "title" <*> v .:   "type" <*> v .:   "can_send_in_message" <*> v .:   "timestamp" <*> v .:   "publisher" <*> v .:   "reblogged_root_name" <*> v .:   "reblogged_from_name" <*> v .:   "description" <*> v .:   "can_reply" <*> v .:   "reblogged_root_title" <*> v .:   "reblogged_from_title" <*> v .:   "tags" <*> v .:   "reblog"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["post_url" .= topLevelPostUrl, "summary" .= topLevelSummary, "note_count" .= topLevelNoteCount, "reblogged_from_id" .= topLevelRebloggedFromId, "display_avatar" .= topLevelDisplayAvatar, "reblogged_root_id" .= topLevelRebloggedRootId, "recommended_source" .= topLevelRecommendedSource, "state" .= topLevelState, "link_author" .= topLevelLinkAuthor, "reblogged_root_url" .= topLevelRebloggedRootUrl, "reblogged_from_url" .= topLevelRebloggedFromUrl, "trail" .= topLevelTrail, "slug" .= topLevelSlug, "url" .= topLevelUrl, "format" .= topLevelFormat, "blog_name" .= topLevelBlogName, "short_url" .= topLevelShortUrl, "reblogged_from_can_message" .= topLevelRebloggedFromCanMessage, "reblogged_root_can_message" .= topLevelRebloggedRootCanMessage, "reblog_key" .= topLevelReblogKey, "date" .= topLevelDate, "reblogged_from_uuid" .= topLevelRebloggedFromUuid, "reblogged_root_uuid" .= topLevelRebloggedRootUuid, "recommended_color" .= topLevelRecommendedColor, "can_reblog" .= topLevelCanReblog, "id" .= topLevelId, "excerpt" .= topLevelExcerpt, "can_like" .= topLevelCanLike, "title" .= topLevelTitle, "type" .= topLevelType, "can_send_in_message" .= topLevelCanSendInMessage, "timestamp" .= topLevelTimestamp, "publisher" .= topLevelPublisher, "reblogged_root_name" .= topLevelRebloggedRootName, "reblogged_from_name" .= topLevelRebloggedFromName, "description" .= topLevelDescription, "can_reply" .= topLevelCanReply, "reblogged_root_title" .= topLevelRebloggedRootTitle, "reblogged_from_title" .= topLevelRebloggedFromTitle, "tags" .= topLevelTags, "reblog" .= topLevelReblog]
  toEncoding (TopLevel {..}) = pairs  ("post_url" .= topLevelPostUrl<>"summary" .= topLevelSummary<>"note_count" .= topLevelNoteCount<>"reblogged_from_id" .= topLevelRebloggedFromId<>"display_avatar" .= topLevelDisplayAvatar<>"reblogged_root_id" .= topLevelRebloggedRootId<>"recommended_source" .= topLevelRecommendedSource<>"state" .= topLevelState<>"link_author" .= topLevelLinkAuthor<>"reblogged_root_url" .= topLevelRebloggedRootUrl<>"reblogged_from_url" .= topLevelRebloggedFromUrl<>"trail" .= topLevelTrail<>"slug" .= topLevelSlug<>"url" .= topLevelUrl<>"format" .= topLevelFormat<>"blog_name" .= topLevelBlogName<>"short_url" .= topLevelShortUrl<>"reblogged_from_can_message" .= topLevelRebloggedFromCanMessage<>"reblogged_root_can_message" .= topLevelRebloggedRootCanMessage<>"reblog_key" .= topLevelReblogKey<>"date" .= topLevelDate<>"reblogged_from_uuid" .= topLevelRebloggedFromUuid<>"reblogged_root_uuid" .= topLevelRebloggedRootUuid<>"recommended_color" .= topLevelRecommendedColor<>"can_reblog" .= topLevelCanReblog<>"id" .= topLevelId<>"excerpt" .= topLevelExcerpt<>"can_like" .= topLevelCanLike<>"title" .= topLevelTitle<>"type" .= topLevelType<>"can_send_in_message" .= topLevelCanSendInMessage<>"timestamp" .= topLevelTimestamp<>"publisher" .= topLevelPublisher<>"reblogged_root_name" .= topLevelRebloggedRootName<>"reblogged_from_name" .= topLevelRebloggedFromName<>"description" .= topLevelDescription<>"can_reply" .= topLevelCanReply<>"reblogged_root_title" .= topLevelRebloggedRootTitle<>"reblogged_from_title" .= topLevelRebloggedFromTitle<>"tags" .= topLevelTags<>"reblog" .= topLevelReblog)




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


