module App.Search where

import Prelude
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Data.Foreign (F, Foreign, readArray, tagOf, toForeign, unsafeFromForeign)
import Data.Foreign.Keys (keys)
import Data.Foreign.Index ((!))
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (genericDecode, genericEncode, defaultOptions)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX, get)

foreign import data LunrIndex :: Type

foreign import _loadIndex :: String -> LunrIndex

loadIndex :: forall e. Aff (ajax :: AJAX | e) (Either Error LunrIndex)
loadIndex = attempt $ do
  res <- get "search_index.json"
  pure $ _loadIndex res.response

foreign import _searchIn :: LunrIndex -> String -> Foreign

searchIn :: LunrIndex -> String -> Array SearchResult
searchIn idx query = either (const []) id $ runExcept $
  readArray (_searchIn idx query) >>= traverse decode

newtype LunrMetadata = LunrMetadata (StrMap (StrMap (StrMap (Array String))))

newtype MatchData = MatchData
  { metadata :: LunrMetadata
  }

instance showLunrMetadata :: Show LunrMetadata where
  show (LunrMetadata m) = show m
instance encodeLunrMetadata :: Encode LunrMetadata where
  encode (LunrMetadata m) = toForeign m
instance decodeLunrMetadata :: Decode LunrMetadata where
  decode value = do
    obj <- readObject (readObject (readObject decode)) value
    pure $ LunrMetadata obj

readObject :: forall a. (Foreign -> F a) -> Foreign -> F (StrMap a)
readObject f value = do
  keys' <- keys value
  pairs <- traverse (\k -> (value ! k) >>= f >>= \v -> pure $ Tuple k v) keys'
  pure $ StrMap.fromFoldable pairs

derive instance newtypeMatchData :: Newtype MatchData _
derive instance genericMatchData :: Generic MatchData _
instance showMatchData :: Show MatchData where
  show (MatchData m) = show m.metadata
instance decodeMatchData :: Decode MatchData where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeMatchData :: Encode MatchData where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}


newtype SearchResult = SearchResult
  { ref :: String
  , score :: Number
  , matchData :: MatchData
  }

derive instance newtypeSearchResult :: Newtype SearchResult _
derive instance genericSearchResult :: Generic SearchResult _
instance decodeSearchResult :: Decode SearchResult where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeSearchResult :: Encode SearchResult where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}


