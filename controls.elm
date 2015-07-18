module Controls (btn) where

import Html exposing (div, label, input, text, Html, fromElement, i, a)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes
import Graphics.Element exposing (Element, image, width)
import Graphics.Input exposing (clickable)
import String

btn : String -> Signal.Address String -> Html
btn action address =
  a
  [onClick address action]

  [
   i
   [
    Html.Attributes.class (String.concat ["fa fa-", action])
   ]
   []
  ]
