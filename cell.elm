module Cell (Model, init, Condition, update, view) where

import Graphics.Collage exposing (rect, filled, move, Form)
import Color

type State = Dead | Alive
type Condition = Underpopulation | Overcrowding | Reproduction | Stable
type alias Model = {x: Int, y: Int, state: State}

init : Int -> Int -> State -> Model
init row col state = 
  {x = row, y = col, state = state}

update : Condition -> Model -> Model
update condition model =
  {model | state <- case condition of
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