import Html exposing (..)
import Html.Attributes exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Keyboard
import Window
import Color
import Set
import Time exposing (..)

import Game


updateInterval = (200 * millisecond)


type Action = Tick | Click (Int, Int) | Resize (Int, Int) | ToggleState


type alias Model =
  { world : Game.World
  , running : Bool
  , boardWidth : Int
  , boardHeight : Int
  }


clock : Signal Time
clock = Time.every updateInterval


clicks : Signal (Int, Int)
clicks = Signal.sampleOn Mouse.clicks Mouse.position


spacePresses : Signal Bool
spacePresses = Signal.filter (\x -> x) False Keyboard.space


actions : Signal Action
actions =
  Signal.mergeMany
    [ Signal.map Click clicks
    , Signal.map (always Tick) clock
    , Signal.map Resize Window.dimensions
    , Signal.map (always ToggleState) spacePresses
    ]


model : Signal Model
model =
  Signal.foldp update initialModel actions


main = Signal.map view model


initialModel : Model
initialModel =
    { world = Game.initialWorld
    , running = False
    , boardWidth = 100
    , boardHeight = 100
    }


update action model =
  case action of
    Tick -> tick model
    Click pos -> click pos model
    Resize (w, h) -> { model | boardWidth <- w, boardHeight <- h }
    ToggleState -> { model | running <- not model.running }


tick model =
  let
      world =
        if model.running
          then Game.evolve model.world
          else model.world
  in
    { model | world <- world }



click (x, y) model =
  let
      dx = model.boardWidth // 2
      dy = model.boardHeight // 2
      offset = pieceSize // 2
      x' = x - dx + offset
      y' = 0 - (y - dy) + offset
      cell = (x' // pieceSize, y' // pieceSize)
      world = Game.toggleCell model.world cell
  in
      { model | world <- world }


pieceSize = 10


renderCell (x, y) =
  let
      piece = filled Color.black (rect pieceSize pieceSize)
      x' = toFloat (x * pieceSize)
      y' = toFloat (y * pieceSize)
  in
    piece
      |> move (x', y')


view model =
  let
      cellViews = List.map renderCell (Set.toList model.world)
  in
      collage model.boardWidth model.boardHeight cellViews
