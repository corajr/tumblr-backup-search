module App.Events where

import Prelude
import App.Routes (Route(..), match)
import App.State (State(..))
import App.Search
import Data.Function (($))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Class (liftEff)
import Pux (EffModel, noEffects, onlyEffects)
import Data.Foreign (toForeign)
import Pux.DOM.Events (DOMEvent, targetValue)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)

data Event = PageView Route
           | Navigate String DOMEvent
           | RequestIndex
           | ReceiveIndex (Either String LunrIndex)
           | SearchQuery DOMEvent
           | SearchResults (Array String)

type AppEffects fx = (ajax :: AJAX, dom :: DOM, history :: HISTORY | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView Home) (State st) = foldp (PageView Search) (State st)
foldp (PageView route@Search) (State st) =
  { state: State st { route = route, status = "Loaded" }
  , effects: [ pure (Just RequestIndex)]
  }
foldp (PageView route) (State st) = noEffects $ State st { route = route, status = "Loaded" }
foldp (Navigate url ev) st =
  onlyEffects st [ liftEff do
                     preventDefault ev
                     h <- history =<< window
                     pushState (toForeign {}) (DocumentTitle "") (URL url) h
                     pure $ Just $ PageView (match url)
                 ]
foldp (ReceiveIndex (Left err)) (State state) =
  noEffects $ State state { status = "Error fetching index: " <> show err }
foldp (ReceiveIndex (Right index)) (State state) =
  noEffects $ State state { index = Just index, status = "Loaded" }
foldp (RequestIndex) (State state) =
  { state: State state { status = "Loading..." }
  , effects: [ do
       res <- loadIndex
       let res' = either (Left <<< show) pure res
       pure $ Just $ ReceiveIndex res'
    ]
  }
foldp (SearchQuery ev) (State state) =
  let query = targetValue ev
  in { state: State state { query = query, status = "Fetching results.." }
     , effects: [ case state.index of
                     Just index -> pure $ Just $ SearchResults (searchIn index query)
                     Nothing -> pure $ Just $ SearchResults []
                ]
     }
foldp (SearchResults results) (State state) =
  noEffects $ State state { results = results }
