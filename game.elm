module Game (view, Model, init, step, play, pause, flipCell) where

import List exposing (map, any, length, filter, concatMap)
import Graphics.Collage exposing (group, Form)
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
linearIndex i j game =
  i * game.rows + j

getCell : Model -> Int -> Int -> Maybe Cell.Model
getCell game i j =
  Array.get (linearIndex i j game) game.cells

flipCell : Model -> (Int, Int) -> Model
flipCell game (i,j) =
  let cell = Cell.flip (getCell game i j) in
  case cell of
    Nothing -> game
    Just cell -> {game | cells <- Array.set (linearIndex i j game) cell game.cells}

neighbours : Cell.Model -> Model -> List (Maybe Cell.Model)
neighbours cell game =
  [
   getCell game (cell.pos.i - 1) (cell.pos.j - 1),
   getCell game cell.pos.i (cell.pos.j - 1),
   getCell game (cell.pos.i + 1) (cell.pos.j - 1),
   getCell game (cell.pos.i - 1) cell.pos.j,
   getCell game (cell.pos.i + 1) cell.pos.j,
   getCell game (cell.pos.i - 1) (cell.pos.j + 1),
   getCell game cell.pos.i (cell.pos.j + 1),
   getCell game (cell.pos.i + 1) (cell.pos.j + 1)
  ]

view : Model -> Int -> Form
view game cellSize =
  group (map (\cell -> (Cell.view cell cellSize)) (Array.toList game.cells))
