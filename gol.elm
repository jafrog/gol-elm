module GoL where

import Signal
import Window
import Html exposing (..)
import Html.Attributes
import Html.Events
import String exposing (..)

import Game

type Action
  = NoOp |
    CellSize String

updates : Signal.Mailbox Action
updates = Signal.mailbox NoOp


view : Game.Game -> (Int, Int) -> Action -> Html
view state (w,h) action =
  let cellSize = case action of
                   NoOp -> toString <| (max w h) // 20
                   CellSize size -> size in
  div [] [
         cellSizeSlider cellSize,
         fromElement <| Game.display state (w,h) cellSize
        ]

main = Signal.map3 view Game.gameState Window.dimensions (updates.signal)

