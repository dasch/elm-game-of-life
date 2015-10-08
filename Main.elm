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


type Action = Evolve | Click (Int, Int) | Resize (Int, Int) | ToggleState
type State = Running | Paused


type alias Model =
  { world : Game.World
  , state : State
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
    , Signal.map (always Evolve) clock
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
    , state = Paused
    , boardWidth = 100
    , boardHeight = 100
    }


update action model =
  case action of
    Evolve -> tick model
    Click pos -> click pos model
    Resize (w, h) -> { model | boardWidth <- w, boardHeight <- h }
    ToggleState -> { model | state <- toggleState model.state }


tick model =
  let
      world = case model.state of
        Running -> Game.evolve model.world
        Paused -> model.world
  in
    { model | world <- world }


toggleState state =
  case state of
    Running -> Paused
    Paused -> Running


click (x, y) model =
  let
      dx = model.boardWidth // 2
      dy = model.boardHeight // 2
      cell = ((x - dx) // pieceSize, 0 - (y - dy) // pieceSize)
      world = Game.toggleCell model.world cell
  in
      { model | world <- world }


pieceSize = 10


piece = filled Color.black (rect pieceSize pieceSize)


renderCell (x, y) =
  let
      x' = toFloat (x * pieceSize - pieceSize // 2)
      y' = toFloat (y * pieceSize + pieceSize // 2)
  in
    piece
      |> move (x', y')


view model =
  let
      cellViews = List.map renderCell (Set.toList model.world)
  in
      collage model.boardWidth model.boardHeight cellViews
