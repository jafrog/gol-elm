import Signal
import Window
import Html (..)
import Html.Attributes
import Html.Events
import String (..)

import Game

type Action
  = NoOp |
    CellSize String

updates : Signal.Channel Action
updates = Signal.channel NoOp

cellSizeSlider : String -> Html
cellSizeSlider cellSize = div
                          [
                           Html.Attributes.style [("margin", "10px")]
                          ]

                          [
                           label [] [ text "Cell size" ],
                           input
                           [Html.Attributes.type' "range",
                            Html.Attributes.value cellSize,
                            Html.Attributes.min "5",
                            Html.Attributes.max "100",
                            Html.Attributes.step 5,
                            Html.Events.on "change" Html.Events.targetValue (Signal.send updates << CellSize)
                           ]
                           []
                          ]

view : Game.Game -> (Int, Int) -> Action -> Html
view state (w,h) action =
  let cellSize = case action of
                   NoOp -> toString <| (max w h) // 20
                   CellSize size -> size in
  div [] [
         cellSizeSlider cellSize,
         fromElement <| Game.display state (w,h) cellSize
        ]

main = Signal.map3 view Game.gameState Window.dimensions (Signal.subscribe updates)

