module GameView where

import Html exposing (Html, div, fromElement, text, h2, a, p)
import Html.Attributes exposing (class, property, style, href, id)
import String
import Window
import Array exposing (Array)
import Time exposing (every, second)
import Mouse
import Graphics.Element exposing (show)
import Graphics.Collage exposing (collage, move)
import Signal exposing (..)
import Signal.Extra exposing (foldp')
import Debug
import Json.Encode as Json

import Game exposing (init, step, flipCell)
import Controls exposing (btn)
import Cell exposing (Model, flip, Position)

type alias Model = {w: Int, h: Int, cellSize: Int, game: Game.Model}
type Updates = Action String | Timestamp Float | Flip (Int, Int) | Dimensions (Int, Int)

mailbox = Signal.mailbox "stop"

port location : String

minCellSize = 20
maxCellSize = 100

init : Updates -> Model
init update =
  case update of
    Dimensions (w,h) -> {w = w,
                         h = h,
                         cellSize = 50,
                         game = Game.init ((h // minCellSize) + 1, (w // minCellSize) + 1) (stateParams location)}
-- init = {w = 1000, h = 1000, cellSize = 50, game = Game.init(40,40)}

getCellPosition : Model -> (Int, Int) -> Maybe Cell.Position
getCellPosition model (x, y) =
  if y < 50
  then Nothing
  else Just {i = ((y - 50) // (model.cellSize + 1)), j = (x // (model.cellSize + 1))}

update : Updates -> Model -> Model
update event model =
  let game = model.game in
  case Debug.watch "event" event of
    Flip (x, y) -> let pos = getCellPosition model (x, y) in
                   case pos of
                     Nothing -> model
                     Just pos -> {model | game <- Game.flipCell game (pos.i,pos.j)}
    Timestamp _ -> {model | game <- Game.step game}
    Dimensions (w,h) -> {model | w <- w, h <- h}
    Action "play" -> {model | game <- Game.play game}
    Action "pause" -> {model | game <- Game.pause game}
    Action "minus" -> updateCellSize model -5
    Action "plus" -> updateCellSize model 5

updateCellSize : Model -> Int -> Model
updateCellSize model diff =
  let cellSize = (model.cellSize + diff) in
  if cellSize >= minCellSize && cellSize <= maxCellSize
  then {model | cellSize <- cellSize}
  else model

stateParams : String -> String
stateParams url =
  case List.tail (String.split "?" url) of
    Nothing -> ""
    Just [] -> ""
    Just [str] -> str

gameStateLink : Game.Model -> String
gameStateLink game =
  game.cells |> Array.filter (\cell -> Cell.isAlive cell)
             |> Array.foldl (\cell string -> string ++ ";" ++ toString cell.pos.i ++ "," ++ toString cell.pos.j) ""
             |> String.append "http://jafrog.com/gol-elm?"

view : Model -> (Int, Int) -> Html
view model (w, h) =
    div [] [
           div
           [
            class "modal",
            id "shareDialog",
            property "aria-hidden" (Json.string "true")
           ]
           [
            div
            [class "modal-dialog"]
            [
             div
             [class "modal-body"]
             [
              a
              [
               href "#close",
               class "btn-close",
               property "aria-hidden" (Json.string "true")
              ]
              [text "x"],

              a [href (gameStateLink model.game), id "share"] [text "SHARE ME"]
             ]
            ]
           ],

           div
           [
            class "controls",
            style [
                   ("height", "50px")
                  ]
           ]
           [
            btn "minus" mailbox.address,
            btn "play" mailbox.address,
            btn "pause" mailbox.address,
            btn "plus" mailbox.address,
            btn "share" mailbox.address
           ],

           [Game.view model.game model.cellSize |> move (-(toFloat w)/2 + (toFloat model.cellSize)/2, (toFloat h)/2 - (toFloat model.cellSize)/2)]
             |> collage w h
             |> fromElement
          ]

updates = Signal.mergeMany [
           Dimensions <~ Window.dimensions,
           Action <~ mailbox.signal,
           Timestamp <~ (every second),
           Flip <~ (Signal.sampleOn Mouse.clicks Mouse.position)
          ]

viewState = foldp' update init updates
-- viewState = foldp update init updates
main = view <~ viewState ~ Window.dimensions