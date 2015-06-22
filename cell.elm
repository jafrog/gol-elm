module Cell (Model, init, Condition, update, view, State) where

import Graphics.Collage exposing (rect, filled, move, Form)
import Color
import List exposing (length, filter)

type State = Dead | Alive
type Condition = Underpopulation | Overcrowding | Reproduction | Stable
type alias Model = {x: Int, y: Int, state: State}

init : Int -> Int -> Model
init row col =
  {x = row, y = col, state = Dead}

update : List (Maybe Model) -> Model -> Model
update neighbours model =
  {model | state <- case condition neighbours of
                      Underpopulation -> Dead
                      Overcrowding -> Dead
                      Reproduction -> Alive
                      Stable -> Alive}

view : Signal.Address Condition -> Model -> Int -> Form
view address model size =
  rect (toFloat size) (toFloat size) |> filled (case model.state of
                                                  Dead -> Color.white
                                                  Alive -> Color.black)
                                     |> move (toFloat <| (model.x + 1) * size, toFloat <| -(model.y + 1) * size)

condition : List (Maybe Model) -> Condition
condition neighbours =
  let aliveNeighbours = length <| filter (\cell ->
                                            case cell of
                                              Nothing -> False
                                              Just cell -> cell.state == Alive) neighbours in
  if | aliveNeighbours < 2 -> Underpopulation
     | aliveNeighbours == 2 -> Stable
     | aliveNeighbours > 3 -> Overcrowding
     | aliveNeighbours == 3 -> Reproduction