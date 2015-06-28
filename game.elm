module Game (view, Model) where

import Debug
import Window
import Mouse
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
import Controls exposing (cellSizeSlider)

type State = Play | Pause
type Action = Start | Stop | Reset | CellSize String
type Updates = UserAction Action | Dimensions (Int, Int) | Timestamp Float | Flip (Int, Int)
type alias Model = {cells: Array Cell.Model, rows: Int, cols: Int, w: Int, h: Int, state: State}

init : (Int, Int) -> Model
init (rows, cols) =
  {
    rows = rows,
    cols = cols,
    w = 1000,
    h = 1000,
    cells = Array.initialize (rows*cols) (\i -> Cell.init (i // rows) (i `rem` rows)),
    state = Pause
  }

mailbox = Signal.mailbox Stop

update : Updates -> Model -> Model
update event game =
  case Debug.watch "event" event of
    Dimensions (width, height) -> {game | w <- width, h <- height}
    Flip (x, y) -> let cell = Cell.flip (findCell game x y) in
                   case cell of
                     Nothing -> game
                     Just cell -> {game | cells <- Array.set (linearIndex cell.x cell.y game) cell game.cells}
    Timestamp _ -> {game | cells <- Array.map (\cell -> Cell.update (neighbours cell game) cell) game.cells}

linearIndex : Int -> Int -> Model -> Int
linearIndex x y game =
  x * game.rows + y * game.cols

findCell : Model -> Int -> Int -> Maybe Cell.Model
findCell game x y =
  getCell game ((x - game.w // 2) // cellSize) ((y + game.h // 2) // cellSize)

getCell : Model -> Int -> Int -> Maybe Cell.Model
getCell game x y =
  Array.get (linearIndex x y game) game.cells

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
cellSize = 10

view : Model -> Html
view game =
  -- let cellSize = case String.toInt cellSizeString of
  --                  Ok result -> result
  --                  Err message -> (max w h) // 20 in
  div [] [
         cellSizeSlider (Signal.forwardTo mailbox.address CellSize) "10",
         collage game.w game.h [
                    group (map (\cell -> Cell.view cell cellSize) (Array.toList game.cells)) |> move (-(toFloat game.w)/2, (toFloat game.h)/2)] |> fromElement
        ]

updates = Signal.mergeMany [
           Signal.map UserAction mailbox.signal,
           Signal.map Dimensions Window.dimensions,
           Signal.map Timestamp (every second),
           Signal.map Flip (Signal.sampleOn Mouse.clicks Mouse.position)
          ]
gameState = Signal.foldp update (init (gameSize, gameSize)) updates
main = Signal.map view gameState