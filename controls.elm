module Controls (cellSizeSlider) where

import Html exposing (div, label, input, text, Html)
import Html.Events exposing (on, targetValue)
import Html.Attributes

cellSizeSlider : Signal.Address String -> String -> Html
cellSizeSlider address cellSize =
  div
  [
   Html.Attributes.style [("margin", "10px")]
  ]

  [
   label [] [ text "Cell size" ],
   input
   [
    Html.Attributes.type' "range",
    Html.Attributes.value cellSize,
    Html.Attributes.min "5",
    Html.Attributes.max "100",
    Html.Attributes.step "5",
    on "change" targetValue (Signal.message address)
   ]
   []
  ]
