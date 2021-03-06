module App.View.Layout where

import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import App.View.Search as Search
import App.Events (Event)
import App.Routes (Route(..))
import App.State (State(..))
import CSS (CSS, fromString, (?), fontSize, display, inlineBlock, marginTop, marginRight, marginLeft, px, value, key, color, backgroundColor, padding, borderRadius)
import CSS.Border (border, solid)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (center, leftTextAlign, textAlign)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css

    case st.route of
      (Home) -> Homepage.view (State st)
      (Search) -> Search.view (State st)
      (NotFound url) -> NotFound.view (State st)

css :: CSS
css = do
  let green = rgb 14 196 172
      blue = rgb 14 154 196
      white = rgb 250 250 250
      black = rgb 0 0 0

  fromString "body" ? do
    backgroundColor white
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    color black
    textAlign leftTextAlign

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (48.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)

  fromString "a" ? do
    display inlineBlock
    borderRadius (2.0 #px) (2.0 #px) (2.0 #px) (2.0 #px)
    padding (6.0 #px) (6.0 #px) (6.0 #px) (6.0 #px)
    textDecoration noneTextDecoration

  fromString ".guide" ? do
    border solid (2.0 #px) green
    color green
    marginRight (10.0 #px)

  fromString ".guide:hover" ? do
    backgroundColor green
    color white

  fromString ".github" ? do
    border solid (2.0 #px) blue
    color blue
    marginLeft (10.0 #px)

  fromString ".github:hover" ? do
    backgroundColor blue
    color white
