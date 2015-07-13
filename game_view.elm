module GameView where

import Html exposing (Html, div, fromElement)
import Html.Attributes
import String
import Window
import Debug
import Array exposing (Array)
import Time exposing (every, second)
import Mouse
import Graphics.Element exposing (show)
import Graphics.Collage exposing (collage, move)

import Game exposing (init, step, getCell, linearIndex)
import Controls exposing (cellSizeSlider, btn)
import Cell exposing (Model, flip)

type alias Model = {w: Int, h: Int, cellSize: Int, game: Game.Model}
type Action = State String | CellSize String
type Updates = UserAction Action | Timestamp Float | Flip (Int, Int) | Dimensions (Int, Int)


mailbox = Signal.mailbox (State "stop")

model = {cellSize = 50, game = Game.init (40,40), w = 1000, h = 1000}

findCell : Model -> Int -> Int -> Maybe Cell.Model
findCell model x y =
  Game.getCell model.game
               (Debug.watch "cell x" (x // (model.cellSize + 1)))
               (Debug.watch "cell y" ((y - 50) // (model.cellSize + 1)))

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
    UserAction (State "play") -> {model | game <- Game.play game}
    UserAction (State "pause") -> {model | game <- Game.pause game}
    UserAction (State "restart") -> {model | game <- Game.init (game.rows, game.cols)}

view : Model -> (Int, Int) -> Html
view model (w, h) =
    div [] [
           div
           [
            Html.Attributes.class "controls",
            Html.Attributes.style [
                   ("height", "50px")
                  ]
           ]
           [
            btn "play" (Signal.forwardTo mailbox.address State),
            btn "pause" (Signal.forwardTo mailbox.address State),
            btn "restart" (Signal.forwardTo mailbox.address State),
            cellSizeSlider (Signal.forwardTo mailbox.address CellSize) (toString model.cellSize)
           ],

           [Game.view model.game model.cellSize |> move (-(toFloat w)/2 + (toFloat model.cellSize)/2, (toFloat h)/2 - (toFloat model.cellSize)/2)]
             |> collage w h
             |> fromElement
          ]

updates = Signal.mergeMany [
           Signal.map UserAction mailbox.signal,
           Signal.map Timestamp (every second),
           Signal.map Flip (Signal.sampleOn Mouse.clicks Mouse.position),
           Signal.map Dimensions Window.dimensions
          ]

viewState = Signal.foldp update model updates
main = Signal.map2 view viewState Window.dimensions