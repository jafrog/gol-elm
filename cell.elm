module Cell (Model, init, Condition, update, view, State, flip) where

import Graphics.Collage exposing (rect, filled, outlined, solid, move, Form)
import Graphics.Input exposing (clickable)
import Color
import Debug
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
                      Stable -> model.state}

view : Model -> Int -> Form
view model size =
  rect (toFloat size) (toFloat size) |> filled (case model.state of
                                                  Dead -> Color.grey
                                                  Alive -> Color.black)
                                     |> move (toFloat <| model.x * size + model.x, toFloat <| -model.y * size - model.y)

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

flip : Maybe Model -> Maybe Model
flip cell =
  case cell of
    Nothing -> Nothing
    Just cell -> if | cell.state == Dead -> Just {cell | state <- Alive}
                    | cell.state == Alive -> Just {cell | state <- Dead}