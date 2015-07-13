module Controls (cellSizeSlider, btn) where

import Html exposing (div, label, input, text, Html, fromElement, i)
import Html.Events exposing (on, targetValue)
import Html.Attributes
import Graphics.Element exposing (Element, image, width)
import Graphics.Input exposing (clickable)
import String

btn : String -> Signal.Address String -> Html
btn action address =
  i
  [
   Html.Attributes.class (String.concat ["fa fa-", action]),
   Html.Attributes.style [
          ("display", "inline-block"),
          ("margin", "10px 10px 0 10px")
         ]
  ]

  [
   image 30 30 (String.concat ["./", action, ".png"]) |>
   clickable (Signal.message address action) |>
   fromElement
  ]

cellSizeSlider : Signal.Address String -> String -> Html
cellSizeSlider address cellSize =
  div
  [
   Html.Attributes.style [("margin", "10px"),
                          ("display", "inline-block")]
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
