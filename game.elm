module Game (view, Model, getCell, init, step, linearIndex, play, pause) where

import List exposing (map, any, length, filter, concatMap)
import Graphics.Collage exposing (group, Form)
import Array exposing (Array)
import Debug

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

play : Model -> Model
play game =
  {game | state <- Play}

pause : Model -> Model
pause game =
  {game | state <- Pause}

step : Model -> Model
step game = if game.state == Play
            then {game | cells <- Array.map (\cell -> Cell.update (neighbours cell game) cell) game.cells}
            else game

linearIndex : Int -> Int -> Model -> Int
linearIndex x y game =
  Debug.watch "linearIndex" <| x * game.rows + y

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

view : Model -> Int -> Form
view game cellSize =
  group (map (\cell -> (Cell.view cell cellSize)) (Array.toList game.cells))
