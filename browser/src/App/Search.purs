module App.Search where

import Prelude
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)

foreign import data LunrIndex :: Type

foreign import _loadIndex :: String -> LunrIndex

loadIndex :: forall e. Aff (ajax :: AJAX | e) (Either Error LunrIndex)
loadIndex = attempt $ do
  res <- get "search_index.json"
  pure $ _loadIndex res.response

foreign import searchIn :: LunrIndex -> String -> Array String
