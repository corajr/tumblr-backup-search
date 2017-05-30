module App.View.Search where

import App.Events (Event(..))
import App.State (State(..))
import App.Search
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (($), (<<<))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange)
import Text.Smolder.HTML (a, div, h1, ul, li, input)
import Text.Smolder.HTML.Attributes (href, target, type', className, value)
import Text.Smolder.Markup ((!), (#!), text)

result :: SearchResult -> HTML Event
result (SearchResult s) =
  li $ a ! href s.ref ! target "_blank" $ text s.ref

view :: State -> HTML Event
view (State s) =
  div do
    h1 $ text "Search"
    div $ text s.status
    input ! type' "text" ! value s.query #! onChange SearchQuery
    ul $ for_ s.results result
