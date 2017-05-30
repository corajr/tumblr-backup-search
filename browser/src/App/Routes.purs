module App.Routes where

import Data.Function (($))
import Data.Functor ((<$))
import Control.Alt ((<|>))
import Control.Applicative ((<*))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit)

data Route = Home | Search | NotFound String

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  Search <$ lit "search" <* end

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Home) = "/"
toURL (Search) = "/search"
