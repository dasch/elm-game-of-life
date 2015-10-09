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


type Action = Tick | Click (Int, Int) | ToggleState


type alias Model =
  { world : Game.World
  , running : Bool
  }


clock : Signal Time
clock = Time.every updateInterval


clicks : Signal (Int, Int)
clicks =
  let
      adjust (w, h) (x, y) =
        let
            x' = x - w // 2
            y' = 0 - y + h // 2
        in
            (x' // pieceSize, y' // pieceSize)
  in
    Signal.sampleOn Mouse.clicks Mouse.position
      |> Signal.map2 adjust Window.dimensions


spacePresses : Signal Bool
spacePresses = Signal.filter (\x -> x) False Keyboard.space


actions : Signal Action
actions =
  Signal.mergeMany
    [ Signal.map Click clicks
    , Signal.map (always Tick) clock
    , Signal.map (always ToggleState) spacePresses
    ]


model : Signal Model
model =
  Signal.foldp update initialModel actions


main = Signal.map2 view Window.dimensions model


initialModel : Model
initialModel =
    { world = Game.initialWorld
    , running = False
    }


update action model =
  case action of
    Tick -> tick model
    Click pos -> click pos model
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
  { model | world <- Game.toggleCell model.world (x, y) }


pieceSize = 10


renderCell (x, y) =
  let
      piece = filled Color.black (rect pieceSize pieceSize)
      x' = toFloat (x * pieceSize)
      y' = toFloat (y * pieceSize)
  in
    piece
      |> move (x', y')


view (w, h) model =
  let
      cellViews = List.map renderCell (Set.toList model.world)
  in
      collage w h cellViews
