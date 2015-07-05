module GameView where

import Html exposing (Html, div, fromElement)
import String
import Window
import Debug
import Array exposing (Array)
import Time exposing (every, second)
import Mouse
import Graphics.Element exposing (show)

import Game exposing (init, step, getCell, linearIndex)
import Controls exposing (cellSizeSlider)
import Cell exposing (Model, flip)

type alias Model = {w: Int, h: Int, cellSize: Int, game: Game.Model}
type Action = Start | Stop | Reset | CellSize String
type Updates = UserAction Action | Timestamp Float | Flip (Int, Int) | Dimensions (Int, Int)


mailbox = Signal.mailbox Stop

model = {w = 1000, h = 1000, cellSize = 50, game = Game.init (20,20)}

findCell : Model -> Int -> Int -> Maybe Cell.Model
findCell model x y =
  Game.getCell model.game ((x - model.w // 2) // model.cellSize) ((y + model.h // 2) // model.cellSize)

update : Updates -> Model -> Model
update event model =
  let game = model.game in
  case Debug.watch "event" event of
    Flip (x, y) -> let cell = Cell.flip (findCell model x y) in
                   case cell of
                     Nothing -> model
                     Just cell -> let game' = {game | cells <- Array.set (Game.linearIndex cell.x cell.y game) cell game.cells} in
                                  {model | game <- game'}
    Timestamp _ -> {model | game <- Game.step game}
    Dimensions (w,h) -> {model | w <- w, h <- h}
    UserAction (CellSize sizeStr) -> let size = String.toInt sizeStr in
                                  case size of
                                    Ok size -> {model | cellSize <- size}
                                    Err msg -> model

view : Model -> Html
view model =
    div [] [
         cellSizeSlider (Signal.forwardTo mailbox.address CellSize) (toString model.cellSize),
         Game.view model.game model.w model.h model.cellSize |> fromElement
        ]

updates = Signal.mergeMany [
           Signal.map UserAction mailbox.signal,
           Signal.map Timestamp (every second),
           Signal.map Flip (Signal.sampleOn Mouse.clicks Mouse.position),
           Signal.map Dimensions Window.dimensions
          ]

viewState = Signal.foldp update model updates
main = Signal.map view viewState