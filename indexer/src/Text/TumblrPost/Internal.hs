{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Text.TumblrPost.Internal where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import           GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data LinkImageDimensions = LinkImageDimensions { 
    linkImageDimensionsHeight :: Double,
    linkImageDimensionsWidth :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON LinkImageDimensions where
  parseJSON (Object v) = LinkImageDimensions <$> v .:   "height" <*> v .:   "width"
  parseJSON _          = mzero


instance ToJSON LinkImageDimensions where
  toJSON     (LinkImageDimensions {..}) = object ["height" .= linkImageDimensionsHeight, "width" .= linkImageDimensionsWidth]
  toEncoding (LinkImageDimensions {..}) = pairs  ("height" .= linkImageDimensionsHeight<>"width" .= linkImageDimensionsWidth)


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
    themeHeaderFocusHeight :: (Maybe (Double:|:[(Maybe Value)])),
    themeBodyFont :: Text,
    themeShowTitle :: Bool,
    themeHeaderImageScaled :: Text,
    themeBackgroundColor :: Text,
    themeHeaderStretch :: Bool,
    themeLinkColor :: Text,
    themeHeaderImageFocused :: Text,
    themeHeaderFocusWidth :: (Maybe (Double:|:[(Maybe Value)])),
    themeShowAvatar :: Bool,
    themeShowHeaderImage :: Bool,
    themeTitleFontWeight :: Text,
    themeHeaderFullHeight :: (Maybe (Double:|:[(Maybe Value)])),
    themeAvatarShape :: Text,
    themeHeaderBounds :: Double:|:Text:|:[(Maybe Value)],
    themeTitleFont :: Text,
    themeHeaderImage :: Text,
    themeTitleColor :: Text,
    themeShowDescription :: Bool,
    themeHeaderFullWidth :: (Maybe (Double:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON Theme where
  parseJSON (Object v) = Theme <$> v .:?? "header_focus_height" <*> v .:   "body_font" <*> v .:   "show_title" <*> v .:   "header_image_scaled" <*> v .:   "background_color" <*> v .:   "header_stretch" <*> v .:   "link_color" <*> v .:   "header_image_focused" <*> v .:?? "header_focus_width" <*> v .:   "show_avatar" <*> v .:   "show_header_image" <*> v .:   "title_font_weight" <*> v .:?? "header_full_height" <*> v .:   "avatar_shape" <*> v .:   "header_bounds" <*> v .:   "title_font" <*> v .:   "header_image" <*> v .:   "title_color" <*> v .:   "show_description" <*> v .:?? "header_full_width"
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
    blogTheme :: Theme:|:[[(Maybe Value)]]
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
    trailEltIsCurrentItem :: (Maybe (Bool:|:[(Maybe Value)])),
    trailEltIsRootItem :: (Maybe (Bool:|:[(Maybe Value)])),
    trailEltContent :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON TrailElt where
  parseJSON (Object v) = TrailElt <$> v .:   "content_raw" <*> v .:   "post" <*> v .:   "blog" <*> v .:?? "is_current_item" <*> v .:?? "is_root_item" <*> v .:   "content"
  parseJSON _          = mzero


instance ToJSON TrailElt where
  toJSON     (TrailElt {..}) = object ["content_raw" .= trailEltContentRaw, "post" .= trailEltPost, "blog" .= trailEltBlog, "is_current_item" .= trailEltIsCurrentItem, "is_root_item" .= trailEltIsRootItem, "content" .= trailEltContent]
  toEncoding (TrailElt {..}) = pairs  ("content_raw" .= trailEltContentRaw<>"post" .= trailEltPost<>"blog" .= trailEltBlog<>"is_current_item" .= trailEltIsCurrentItem<>"is_root_item" .= trailEltIsRootItem<>"content" .= trailEltContent)


data DialogueElt = DialogueElt { 
    dialogueEltName :: Text,
    dialogueEltPhrase :: Text,
    dialogueEltLabel :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON DialogueElt where
  parseJSON (Object v) = DialogueElt <$> v .:   "name" <*> v .:   "phrase" <*> v .:   "label"
  parseJSON _          = mzero


instance ToJSON DialogueElt where
  toJSON     (DialogueElt {..}) = object ["name" .= dialogueEltName, "phrase" .= dialogueEltPhrase, "label" .= dialogueEltLabel]
  toEncoding (DialogueElt {..}) = pairs  ("name" .= dialogueEltName<>"phrase" .= dialogueEltPhrase<>"label" .= dialogueEltLabel)


data Exif = Exif { 
    exifISO :: (Maybe (Double:|:[(Maybe Value)])),
    exifAperture :: (Maybe (Text:|:[(Maybe Value)])),
    exifFocalLength :: (Maybe (Text:|:[(Maybe Value)])),
    exifExposure :: (Maybe (Text:|:[(Maybe Value)])),
    exifCamera :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON Exif where
  parseJSON (Object v) = Exif <$> v .:?? "ISO" <*> v .:?? "Aperture" <*> v .:?? "FocalLength" <*> v .:?? "Exposure" <*> v .:   "Camera"
  parseJSON _          = mzero


instance ToJSON Exif where
  toJSON     (Exif {..}) = object ["ISO" .= exifISO, "Aperture" .= exifAperture, "FocalLength" .= exifFocalLength, "Exposure" .= exifExposure, "Camera" .= exifCamera]
  toEncoding (Exif {..}) = pairs  ("ISO" .= exifISO<>"Aperture" .= exifAperture<>"FocalLength" .= exifFocalLength<>"Exposure" .= exifExposure<>"Camera" .= exifCamera)


data OriginalSize = OriginalSize { 
    originalSizeHeight :: Double,
    originalSizeUrl :: Text,
    originalSizeWidth :: Double
  } deriving (Show,Eq,Generic)


instance FromJSON OriginalSize where
  parseJSON (Object v) = OriginalSize <$> v .:   "height" <*> v .:   "url" <*> v .:   "width"
  parseJSON _          = mzero


instance ToJSON OriginalSize where
  toJSON     (OriginalSize {..}) = object ["height" .= originalSizeHeight, "url" .= originalSizeUrl, "width" .= originalSizeWidth]
  toEncoding (OriginalSize {..}) = pairs  ("height" .= originalSizeHeight<>"url" .= originalSizeUrl<>"width" .= originalSizeWidth)


data PhotosElt = PhotosElt { 
    photosEltCaption :: Text,
    photosEltExif :: (Maybe (Exif:|:[(Maybe Value)])),
    photosEltAltSizes :: [OriginalSize:|:[(Maybe Value)]],
    photosEltOriginalSize :: OriginalSize
  } deriving (Show,Eq,Generic)


instance FromJSON PhotosElt where
  parseJSON (Object v) = PhotosElt <$> v .:   "caption" <*> v .:?? "exif" <*> v .:   "alt_sizes" <*> v .:   "original_size"
  parseJSON _          = mzero


instance ToJSON PhotosElt where
  toJSON     (PhotosElt {..}) = object ["caption" .= photosEltCaption, "exif" .= photosEltExif, "alt_sizes" .= photosEltAltSizes, "original_size" .= photosEltOriginalSize]
  toEncoding (PhotosElt {..}) = pairs  ("caption" .= photosEltCaption<>"exif" .= photosEltExif<>"alt_sizes" .= photosEltAltSizes<>"original_size" .= photosEltOriginalSize)


data PlayerElt = PlayerElt { 
    playerEltWidth :: Double,
    playerEltEmbedCode :: Bool:|:Text:|:[(Maybe Value)]
  } deriving (Show,Eq,Generic)


instance FromJSON PlayerElt where
  parseJSON (Object v) = PlayerElt <$> v .:   "width" <*> v .:   "embed_code"
  parseJSON _          = mzero


instance ToJSON PlayerElt where
  toJSON     (PlayerElt {..}) = object ["width" .= playerEltWidth, "embed_code" .= playerEltEmbedCode]
  toEncoding (PlayerElt {..}) = pairs  ("width" .= playerEltWidth<>"embed_code" .= playerEltEmbedCode)


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
    topLevelLinkImageDimensions :: (Maybe (LinkImageDimensions:|:[(Maybe Value)])),
    topLevelNoteCount :: Double,
    topLevelRebloggedFromId :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelLinkUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelDisplayAvatar :: Bool,
    topLevelRebloggedRootId :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelTrack :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRecommendedSource :: (Maybe Value),
    topLevelState :: Text,
    topLevelHtml5Capable :: (Maybe (Bool:|:[(Maybe Value)])),
    topLevelLinkAuthor :: (Maybe Value),
    topLevelRebloggedRootUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRebloggedFromUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelSourceTitle :: (Maybe (Bool:|:Text:|:[(Maybe Value)])),
    topLevelTrail :: (Maybe ([TrailElt:|:[(Maybe Value)]])),
    topLevelAudioUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelSlug :: Text,
    topLevelVideoUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelText :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelAudioSourceUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelThumbnailUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelBody :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelDialogue :: (Maybe ([DialogueElt])),
    topLevelLinkImage :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelIsExternal :: (Maybe (Bool:|:[(Maybe Value)])),
    topLevelUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelImagePermalink :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelFormat :: Text,
    topLevelEmbed :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelCaption :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelVideoType :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelBlogName :: Text,
    topLevelShortUrl :: Text,
    topLevelAudioType :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRebloggedFromCanMessage :: (Maybe (Bool:|:[(Maybe Value)])),
    topLevelAskingName :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRebloggedRootCanMessage :: (Maybe (Bool:|:[(Maybe Value)])),
    topLevelReblogKey :: Text,
    topLevelDate :: Text,
    topLevelAnswer :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelThumbnailHeight :: (Maybe (Double:|:[(Maybe Value)])),
    topLevelRebloggedFromUuid :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRebloggedRootUuid :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelPhotos :: (Maybe ([PhotosElt])),
    topLevelAlbum :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelAlbumArt :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelTrackName :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelPlays :: (Maybe (Double:|:[(Maybe Value)])),
    topLevelRecommendedColor :: (Maybe Value),
    topLevelCanReblog :: Bool,
    topLevelSource :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelThumbnailWidth :: (Maybe (Double:|:[(Maybe Value)])),
    topLevelId :: Integer,
    topLevelExcerpt :: (Maybe Value),
    topLevelAskingUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelCanLike :: Bool,
    topLevelTitle :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelType :: Text,
    topLevelCanSendInMessage :: Bool,
    topLevelPlayer :: (Maybe (Text:|:[PlayerElt])),
    topLevelPhotosetLayout :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelDuration :: (Maybe (Double:|:[(Maybe Value)])),
    topLevelTimestamp :: Integer,
    topLevelPublisher :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRebloggedRootName :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRebloggedFromName :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelQuestion :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelDescription :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelCanReply :: Bool,
    topLevelRebloggedRootTitle :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelRebloggedFromTitle :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelArtist :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelSourceUrl :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelTags :: [Text:|:[(Maybe Value)]],
    topLevelReblog :: (Maybe (Reblog:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "post_url" <*> v .:   "summary" <*> v .:?? "link_image_dimensions" <*> v .:   "note_count" <*> v .:?? "reblogged_from_id" <*> v .:?? "link_url" <*> v .:   "display_avatar" <*> v .:?? "reblogged_root_id" <*> v .:?? "track" <*> v .:?? "recommended_source" <*> v .:   "state" <*> v .:?? "html5_capable" <*> v .:?? "link_author" <*> v .:?? "reblogged_root_url" <*> v .:?? "reblogged_from_url" <*> v .:?? "source_title" <*> v .:?? "trail" <*> v .:?? "audio_url" <*> v .:   "slug" <*> v .:?? "video_url" <*> v .:?? "text" <*> v .:?? "audio_source_url" <*> v .:?? "thumbnail_url" <*> v .:?? "body" <*> v .:?? "dialogue" <*> v .:?? "link_image" <*> v .:?? "is_external" <*> v .:?? "url" <*> v .:?? "image_permalink" <*> v .:   "format" <*> v .:?? "embed" <*> v .:?? "caption" <*> v .:?? "video_type" <*> v .:   "blog_name" <*> v .:   "short_url" <*> v .:?? "audio_type" <*> v .:?? "reblogged_from_can_message" <*> v .:?? "asking_name" <*> v .:?? "reblogged_root_can_message" <*> v .:   "reblog_key" <*> v .:   "date" <*> v .:?? "answer" <*> v .:?? "thumbnail_height" <*> v .:?? "reblogged_from_uuid" <*> v .:?? "reblogged_root_uuid" <*> v .:?? "photos" <*> v .:?? "album" <*> v .:?? "album_art" <*> v .:?? "track_name" <*> v .:?? "plays" <*> v .:?? "recommended_color" <*> v .:   "can_reblog" <*> v .:?? "source" <*> v .:?? "thumbnail_width" <*> v .:   "id" <*> v .:?? "excerpt" <*> v .:?? "asking_url" <*> v .:   "can_like" <*> v .:?? "title" <*> v .:   "type" <*> v .:   "can_send_in_message" <*> v .:?? "player" <*> v .:?? "photoset_layout" <*> v .:?? "duration" <*> v .:   "timestamp" <*> v .:?? "publisher" <*> v .:?? "reblogged_root_name" <*> v .:?? "reblogged_from_name" <*> v .:?? "question" <*> v .:?? "description" <*> v .:   "can_reply" <*> v .:?? "reblogged_root_title" <*> v .:?? "reblogged_from_title" <*> v .:?? "artist" <*> v .:?? "source_url" <*> v .:   "tags" <*> v .:?? "reblog"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["post_url" .= topLevelPostUrl, "summary" .= topLevelSummary, "link_image_dimensions" .= topLevelLinkImageDimensions, "note_count" .= topLevelNoteCount, "reblogged_from_id" .= topLevelRebloggedFromId, "link_url" .= topLevelLinkUrl, "display_avatar" .= topLevelDisplayAvatar, "reblogged_root_id" .= topLevelRebloggedRootId, "track" .= topLevelTrack, "recommended_source" .= topLevelRecommendedSource, "state" .= topLevelState, "html5_capable" .= topLevelHtml5Capable, "link_author" .= topLevelLinkAuthor, "reblogged_root_url" .= topLevelRebloggedRootUrl, "reblogged_from_url" .= topLevelRebloggedFromUrl, "source_title" .= topLevelSourceTitle, "trail" .= topLevelTrail, "audio_url" .= topLevelAudioUrl, "slug" .= topLevelSlug, "video_url" .= topLevelVideoUrl, "text" .= topLevelText, "audio_source_url" .= topLevelAudioSourceUrl, "thumbnail_url" .= topLevelThumbnailUrl, "body" .= topLevelBody, "dialogue" .= topLevelDialogue, "link_image" .= topLevelLinkImage, "is_external" .= topLevelIsExternal, "url" .= topLevelUrl, "image_permalink" .= topLevelImagePermalink, "format" .= topLevelFormat, "embed" .= topLevelEmbed, "caption" .= topLevelCaption, "video_type" .= topLevelVideoType, "blog_name" .= topLevelBlogName, "short_url" .= topLevelShortUrl, "audio_type" .= topLevelAudioType, "reblogged_from_can_message" .= topLevelRebloggedFromCanMessage, "asking_name" .= topLevelAskingName, "reblogged_root_can_message" .= topLevelRebloggedRootCanMessage, "reblog_key" .= topLevelReblogKey, "date" .= topLevelDate, "answer" .= topLevelAnswer, "thumbnail_height" .= topLevelThumbnailHeight, "reblogged_from_uuid" .= topLevelRebloggedFromUuid, "reblogged_root_uuid" .= topLevelRebloggedRootUuid, "photos" .= topLevelPhotos, "album" .= topLevelAlbum, "album_art" .= topLevelAlbumArt, "track_name" .= topLevelTrackName, "plays" .= topLevelPlays, "recommended_color" .= topLevelRecommendedColor, "can_reblog" .= topLevelCanReblog, "source" .= topLevelSource, "thumbnail_width" .= topLevelThumbnailWidth, "id" .= topLevelId, "excerpt" .= topLevelExcerpt, "asking_url" .= topLevelAskingUrl, "can_like" .= topLevelCanLike, "title" .= topLevelTitle, "type" .= topLevelType, "can_send_in_message" .= topLevelCanSendInMessage, "player" .= topLevelPlayer, "photoset_layout" .= topLevelPhotosetLayout, "duration" .= topLevelDuration, "timestamp" .= topLevelTimestamp, "publisher" .= topLevelPublisher, "reblogged_root_name" .= topLevelRebloggedRootName, "reblogged_from_name" .= topLevelRebloggedFromName, "question" .= topLevelQuestion, "description" .= topLevelDescription, "can_reply" .= topLevelCanReply, "reblogged_root_title" .= topLevelRebloggedRootTitle, "reblogged_from_title" .= topLevelRebloggedFromTitle, "artist" .= topLevelArtist, "source_url" .= topLevelSourceUrl, "tags" .= topLevelTags, "reblog" .= topLevelReblog]
  toEncoding (TopLevel {..}) = pairs  ("post_url" .= topLevelPostUrl<>"summary" .= topLevelSummary<>"link_image_dimensions" .= topLevelLinkImageDimensions<>"note_count" .= topLevelNoteCount<>"reblogged_from_id" .= topLevelRebloggedFromId<>"link_url" .= topLevelLinkUrl<>"display_avatar" .= topLevelDisplayAvatar<>"reblogged_root_id" .= topLevelRebloggedRootId<>"track" .= topLevelTrack<>"recommended_source" .= topLevelRecommendedSource<>"state" .= topLevelState<>"html5_capable" .= topLevelHtml5Capable<>"link_author" .= topLevelLinkAuthor<>"reblogged_root_url" .= topLevelRebloggedRootUrl<>"reblogged_from_url" .= topLevelRebloggedFromUrl<>"source_title" .= topLevelSourceTitle<>"trail" .= topLevelTrail<>"audio_url" .= topLevelAudioUrl<>"slug" .= topLevelSlug<>"video_url" .= topLevelVideoUrl<>"text" .= topLevelText<>"audio_source_url" .= topLevelAudioSourceUrl<>"thumbnail_url" .= topLevelThumbnailUrl<>"body" .= topLevelBody<>"dialogue" .= topLevelDialogue<>"link_image" .= topLevelLinkImage<>"is_external" .= topLevelIsExternal<>"url" .= topLevelUrl<>"image_permalink" .= topLevelImagePermalink<>"format" .= topLevelFormat<>"embed" .= topLevelEmbed<>"caption" .= topLevelCaption<>"video_type" .= topLevelVideoType<>"blog_name" .= topLevelBlogName<>"short_url" .= topLevelShortUrl<>"audio_type" .= topLevelAudioType<>"reblogged_from_can_message" .= topLevelRebloggedFromCanMessage<>"asking_name" .= topLevelAskingName<>"reblogged_root_can_message" .= topLevelRebloggedRootCanMessage<>"reblog_key" .= topLevelReblogKey<>"date" .= topLevelDate<>"answer" .= topLevelAnswer<>"thumbnail_height" .= topLevelThumbnailHeight<>"reblogged_from_uuid" .= topLevelRebloggedFromUuid<>"reblogged_root_uuid" .= topLevelRebloggedRootUuid<>"photos" .= topLevelPhotos<>"album" .= topLevelAlbum<>"album_art" .= topLevelAlbumArt<>"track_name" .= topLevelTrackName<>"plays" .= topLevelPlays<>"recommended_color" .= topLevelRecommendedColor<>"can_reblog" .= topLevelCanReblog<>"source" .= topLevelSource<>"thumbnail_width" .= topLevelThumbnailWidth<>"id" .= topLevelId<>"excerpt" .= topLevelExcerpt<>"asking_url" .= topLevelAskingUrl<>"can_like" .= topLevelCanLike<>"title" .= topLevelTitle<>"type" .= topLevelType<>"can_send_in_message" .= topLevelCanSendInMessage<>"player" .= topLevelPlayer<>"photoset_layout" .= topLevelPhotosetLayout<>"duration" .= topLevelDuration<>"timestamp" .= topLevelTimestamp<>"publisher" .= topLevelPublisher<>"reblogged_root_name" .= topLevelRebloggedRootName<>"reblogged_from_name" .= topLevelRebloggedFromName<>"question" .= topLevelQuestion<>"description" .= topLevelDescription<>"can_reply" .= topLevelCanReply<>"reblogged_root_title" .= topLevelRebloggedRootTitle<>"reblogged_from_title" .= topLevelRebloggedFromTitle<>"artist" .= topLevelArtist<>"source_url" .= topLevelSourceUrl<>"tags" .= topLevelTags<>"reblog" .= topLevelReblog)




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


