module Game (view, Model, init, step, play, pause, flipCell) where

import List exposing (map, any, length, filter, concatMap)
import Graphics.Collage exposing (group, Form)
import Array exposing (Array)
import String exposing (isEmpty, split)
import Debug

import Cell

type State = Play | Pause
type alias Model = {cells: Array Cell.Model, rows: Int, cols: Int, state: State}

preInit : (Int, Int) -> Model
preInit (rows, cols) =
  {
    rows = rows,
    cols = cols,
    cells = Array.initialize (rows*cols) (\i -> Cell.init (i // rows) (i `rem` rows)),
    state = Pause
  }

init : (Int, Int) -> String -> Model
init (rows, cols) stateParams =
  preInit (rows, cols) |> initFromStateParams stateParams


play : Model -> Model
play game =
  {game | state <- Play}

pause : Model -> Model
pause game =
  {game | state <- Pause}

step : Model -> Model
step game = if | allDead game -> {game | state <- Pause}
               | (Debug.watch "state" game.state == Play) -> {game | cells <- Array.map (\cell -> Cell.update (neighbours cell game) cell) game.cells}
               | otherwise -> game

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

allDead : Model -> Bool
allDead game =
  not (any Cell.isAlive (Array.toList game.cells))

initFromStateParams : String -> Model -> Model
initFromStateParams stateParams game =
  if isEmpty stateParams
  then game
  else List.foldl (\pair game -> case pair of
                                   Nothing -> game
                                   Just pair -> flipCell game pair) game (parseStateParams stateParams)

parseStateParams : String -> List (Maybe (Int, Int))
parseStateParams stateParams =
  map parsePair (split ";" stateParams)

parsePair : String -> Maybe (Int, Int)
parsePair pair =
  case List.map (\int -> String.toInt int) (split "," pair) of
    Ok hd :: tl -> case tl of
                     Ok tlhd :: tltl -> Just (hd,tlhd)
                     Err str :: tltl -> Nothing
    Err str :: tl -> Nothing

view : Model -> Int -> Form
view game cellSize =
  group (map (\cell -> (Cell.view cell cellSize)) (Array.toList game.cells))
