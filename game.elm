module Game where

import Debug
import Window
import Color
import List (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Time (..)
import Signal
import Array
import String

type State = Dead | Alive
type Condition = Underpopulation | Overcrowding | Reproduction | Stable
type alias Cell = {x: Int, y: Int, state: State}
type alias Game = {cells: Array.Array (Array.Array Cell), rows: Int, cols: Int}

black = Color.rgb 0 0 0

-- Helpers

inRange : Int -> Int -> Int -> Bool
inRange x y dim2 =
  x >= 1 && y >= 1 &&
  x <= sqrt (toFloat dim2) && y <= sqrt (toFloat dim2)

-- Game

aliveCells = [[0,0], [0,1], [1,0]]

initGame : (Int, Int) -> Game
initGame (rows, cols) =
  {
    rows = rows,
    cols = cols,
    cells = Array.fromList <| map (\row ->
                                     Array.fromList <| map (\col -> {x = row, y = col, state = (initCell row col)}) [0..cols-1])
            [0..rows-1]
  }

updateGame : Float -> Game -> Game
updateGame timestamp game =
  {game | cells <- Array.map (\row -> Array.map (\cell -> updateCell cell game) row) game.cells}

getCell : Game -> Int -> Int -> Maybe Cell
getCell game x y =
  case Array.get x game.cells of
    Nothing -> Nothing
    Just row -> Array.get y row

-- Cell

initCell : Int -> Int -> State
initCell row col = 
  case (any (\[x,y] -> x == row && y == col) aliveCells) of
    True -> Alive
    False -> Dead

updateCell : Cell -> Game -> Cell
updateCell cell game =
  {cell | state <- case cellCondition <| neighbours cell game of
                     Underpopulation -> Dead
                     Overcrowding -> Dead
                     Reproduction -> Alive
                     Stable -> Alive}

neighbours : Cell -> Game -> List (Maybe Cell)
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

cellCondition : List (Maybe Cell) -> Condition
cellCondition neighbours =
  let aliveNeighbours = length <| filter (\cell ->
                                            case cell of
                                              Nothing -> False
                                              Just cell -> cell.state == Alive) neighbours in
  if | aliveNeighbours < 2 -> Underpopulation
     | aliveNeighbours == 2 -> Stable
     | aliveNeighbours > 3 -> Overcrowding
     | aliveNeighbours == 3 -> Reproduction

-- View

gameSize = 20

displayCell : Cell -> Int -> Form
displayCell cell size =
  rect (toFloat size) (toFloat size) |> filled (case cell.state of
                                                  Dead -> Color.white
                                                  Alive -> Color.black)
                                     |> move (toFloat <| (cell.x + 1) * size, toFloat <| -(cell.y + 1) * size)

display : Game -> (Int, Int) -> String -> Element
display game (w, h) cellSizeString =
  let cellSize = case String.toInt cellSizeString of
                   Ok result -> result
                   Err message -> (max w h) // 20 in
  collage w h
            [
             group (map (\cell -> displayCell cell cellSize) (concatMap Array.toList (Array.toList game.cells))) |> move (-(toFloat w)/2, (toFloat h)/2)
            ]

gameState : Signal Game
gameState = Signal.foldp updateGame (initGame (gameSize, gameSize)) (every second)
