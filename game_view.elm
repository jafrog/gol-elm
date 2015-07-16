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

import Game exposing (init, step, getCell, linearIndex, adjust)
import Controls exposing (btn)
import Cell exposing (Model, flip)

type alias Model = {w: Int, h: Int, cellSize: Int, game: Game.Model}
type Updates = Action String | Timestamp Float | Flip (Int, Int) | Dimensions (Int, Int)


mailbox = Signal.mailbox "stop"

model = {cellSize = 50, game = Game.init (20,20), w = 500, h = 500}

findCell : Model -> Int -> Int -> Maybe Cell.Model
findCell model x y =
  Game.getCell model.game
               (x // (model.cellSize + 1))
               ((y - 50) // (model.cellSize + 1))

update : Updates -> Model -> Model
update event model =
  let game = model.game in
  case event of
    Flip (x, y) -> let cell = Cell.flip (findCell model x y) in
                   case cell of
                     Nothing -> model
                     Just cell -> let game' = {game | cells <- Array.set (Game.linearIndex cell.x cell.y game) cell game.cells} in
                                  {model | game <- game'}
    Timestamp _ -> {model | game <- Game.step game}
    Dimensions (w,h) -> {model | w <- w, h <- h}
    Action "play" -> {model | game <- Game.play game}
    Action "pause" -> {model | game <- Game.pause game}
    Action "undo" -> {model | game <- Game.init (game.rows, game.cols)}
    Action "search-minus" -> updateCellSize model -5
    Action "search-plus" -> updateCellSize model 5

updateCellSize : Model -> Int -> Model
updateCellSize model diff =
  let cellSize = Debug.watch "new cellSize" (model.cellSize + diff)
      rows = Debug.watch "new rows" (model.w // cellSize)
      cols = Debug.watch "new cols" (model.h // cellSize) in
  if cellSize >= 5 && cellSize <= 100
  then {model | game <- Game.adjust model.game rows cols,
                        cellSize <- cellSize}
  else model

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
            btn "minus" mailbox.address,
            btn "play" mailbox.address,
            btn "pause" mailbox.address,
            btn "undo" mailbox.address,
            btn "plus" mailbox.address
           ],

           [Game.view model.game model.cellSize |> move (-(toFloat w)/2 + (toFloat model.cellSize)/2, (toFloat h)/2 - (toFloat model.cellSize)/2)]
             |> collage w h
             |> fromElement
          ]

updates = Signal.mergeMany [
           Signal.map Action mailbox.signal,
           Signal.map Timestamp (every second),
           Signal.map Flip (Signal.sampleOn Mouse.clicks Mouse.position),
           Signal.map Dimensions Window.dimensions
          ]

viewState = Signal.foldp update model updates
main = Signal.map2 view viewState Window.dimensions