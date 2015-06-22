module Game where

import Debug
import Window
-- import Color
import List exposing (map, any, length, filter, concatMap)
import Graphics.Collage exposing (collage, group, move)
-- import Graphics.Element exposing (..)
import Time exposing (every, second)
-- import Signal
import Array exposing (Array)
-- import String
import Html exposing (Html, div, fromElement)

import Cell

type Action = Update | Start | Stop | Reset | Condition Cell.Condition | Dimensions (Int, Int)
type alias Model = {cells: Array (Array Cell.Model), rows: Int, cols: Int, w: Int, h: Int}

init : (Int, Int) -> Model
init (rows, cols) =
  {
    rows = rows,
    cols = cols,
    w = 100,
    h = 100,
    cells = Array.fromList <| map (\row -> Array.fromList <|
                                           map (\col -> (Cell.init row col)) [0..cols-1])
                              [0..rows-1]
  }

mailbox = Signal.mailbox Update

step : Float -> Model -> Model
step timestamp game =
  {game | cells <- Array.map (\row -> Array.map (\cell -> Cell.update (neighbours cell game) cell) row) game.cells}

update : Action -> (Int, Int) -> Model -> Model
update action (w, h) game =
  case Debug.watch "action" action of
    Update -> {game | cells <- Array.map (\row -> Array.map (\cell -> Cell.update (neighbours cell game) cell) row) game.cells}
    Dimensions (width, height) -> {game | w <- width, h <- height}

getCell : Model -> Int -> Int -> Maybe Cell.Model
getCell game x y =
  case Array.get x game.cells of
    Nothing -> Nothing
    Just row -> Array.get y row

neighbours : Cell.Model -> Model -> List (Maybe Cell.Model)
neighbours cell game =
  [
   getCell game (cell.x - 1) (cell.y - 1),
   getCell game cell.x (cell.y - 1),
   getCell game (cell.x + 1) (cell.y - 1),
   getCell game (cell.x - 1) cell.y,
   getCell game (cell.x + 1) cell.y,
   getCell game (cell.x - 1) (cell.y + 1),
   getCell game cell.x (cell.y + 1),
   getCell game (cell.x + 1) (cell.y + 1)
  ]

gameSize = 20

view : Model -> Html
view game =
  -- let cellSize = case String.toInt cellSizeString of
  --                  Ok result -> result
  --                  Err message -> (max w h) // 20 in
  div [] [
         fromElement <| collage (Debug.watch "w" game.w) game.h
                   [
                    group (map (\cell -> Cell.view (Signal.forwardTo mailbox.address Condition) cell 10) (concatMap Array.toList (Array.toList game.cells))) |> move (-(toFloat game.w)/2, (toFloat game.h)/2)
                   ]
        ]

gameState = Signal.foldp step (init (gameSize, gameSize)) (every second)
main = Signal.map view (Signal.map3 update mailbox.signal Window.dimensions gameState)
