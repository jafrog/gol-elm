module Game (view, Model, getCell, init, step, linearIndex) where

import List exposing (map, any, length, filter, concatMap)
import Graphics.Collage exposing (collage, group, move)
import Graphics.Element exposing (Element)
import Array exposing (Array)

import Cell

type State = Play | Pause
type alias Model = {cells: Array Cell.Model, rows: Int, cols: Int, state: State}

init : (Int, Int) -> Model
init (rows, cols) =
  {
    rows = rows,
    cols = cols,
    cells = Array.initialize (rows*cols) (\i -> Cell.init (i // rows) (i `rem` rows)),
    state = Pause
  }

step : Model -> Model
step game = {game | cells <- Array.map (\cell -> Cell.update (neighbours cell game) cell) game.cells}
  
linearIndex : Int -> Int -> Model -> Int
linearIndex x y game =
  x * game.rows + y * game.cols

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

cellSize = 10

view : Model -> Int -> Int -> Int -> Element
view game w h cellSize =
  collage w h [
             group (map (\cell -> (Cell.view cell cellSize)) (Array.toList game.cells)) |> move (-(toFloat w)/2 + 10, (toFloat h)/2 - 10)]
