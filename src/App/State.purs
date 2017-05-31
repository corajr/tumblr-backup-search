module App.State where

import App.Config (config)
import App.Routes (Route, match)
import App.Search
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..))

newtype State = State
  { title :: String
  , route :: Route
  , status :: String
  , index :: Maybe LunrIndex
  , query :: String
  , results :: Array SearchResult
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , status: "Not loaded"
  , query: ""
  , index: Nothing
  , results: []
  }
