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

type Action = Start | Stop | Reset | Condition Cell.State
type Updates = UserAction Action | Dimensions (Int, Int) | Timestamp Float
type alias Model = {cells: Array (Array Cell.Model), rows: Int, cols: Int, w: Int, h: Int}

init : (Int, Int) -> Model
init (rows, cols) =
  {
    rows = rows,
    cols = cols,
    w = 1000,
    h = 1000,
    cells = Array.fromList <| map (\row -> Array.fromList <|
                                           map (\col -> (Cell.init row col)) [0..cols-1])
                              [0..rows-1]
  }

mailbox = Signal.mailbox Stop

-- step : Float -> Model -> Model
-- step timestamp game =
--   {game | cells <- Array.map (\row -> Array.map (\cell -> Cell.update (neighbours cell game) cell) row) game.cells}

update : Updates -> Model -> Model
update event game =
  case Debug.watch "event" event of
    Dimensions (width, height) -> {game | w <- width, h <- height}
    Timestamp _ -> {game | cells <- Array.map (\row -> Array.map (\cell -> Cell.update (neighbours cell game) cell) row) game.cells}

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

updates = Signal.mergeMany [
           Signal.map UserAction mailbox.signal,
           Signal.map Dimensions Window.dimensions,
           Signal.map Timestamp (every second)
          ]
gameState = Signal.foldp update (init (gameSize, gameSize)) updates
main = Signal.map view gameState