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


-- The width and height of each piece on the board:
pieceSize = 10


-- Model:


type alias Model = { world : Game.World, running : Bool }


initialModel : Model
initialModel = { world = Game.initialWorld, running = False }


model : Signal Model
model =
  Signal.foldp update initialModel actions


update action model =
  case action of
    Tick ->
      { model | world <- if model.running then Game.evolve model.world else model.world }

    Click (x, y) ->
      { model | world <- Game.toggleCell model.world (x, y) }

    ToggleState ->
      { model | running <- not model.running }


-- Actions:


updateInterval = (200 * millisecond)


type Action = Tick | Click (Int, Int) | ToggleState


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
      -- Sample the mouse position on each click, then adjust the coordinates
      -- to match the game coordinate system.
      Signal.sampleOn Mouse.clicks Mouse.position
        |> Signal.map2 adjust Window.dimensions


spacePresses : Signal ()
spacePresses =
  -- `Keyboard.space` is a signal of Bool, where the value indicates whether the
  -- space key is pressed. We only want to fire when that value is true, so we
  -- filter out all False values.
  Signal.filter identity False Keyboard.space
    |> Signal.map (always ()) -- Drop the actual value, it's not interesting.


actions : Signal Action
actions =
  Signal.mergeMany
    [ Signal.map Click clicks
    , Signal.map (always Tick) (Time.every updateInterval)
    , Signal.map (always ToggleState) spacePresses
    ]


-- Rendering:


main = Signal.map2 view Window.dimensions model


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
