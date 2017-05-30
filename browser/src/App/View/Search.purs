module App.View.Search where

import App.Events (Event(..))
import App.State (State(..))
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (($), (<<<))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onChange)
import Text.Smolder.HTML (a, div, h1, ul, li, input)
import Text.Smolder.HTML.Attributes (href, type', className, value)
import Text.Smolder.Markup ((!), (#!), text)

view :: State -> HTML Event
view (State s) =
  div do
    h1 $ text "Search"
    div $ text s.status
    input ! type' "text" ! value s.query #! onChange SearchQuery
    ul $ for_ s.results (li <<< text)
