module App.Search where

import Prelude
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Foreign (Foreign, readArray)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (genericDecode, genericEncode, defaultOptions)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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

newtype MatchData = MatchData
  { metadata :: Foreign
  }

derive instance newtypeMatchData :: Newtype MatchData _
derive instance genericMatchData :: Generic MatchData _
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


