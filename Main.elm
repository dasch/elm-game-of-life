import Graphics.Collage as Collage
import Graphics.Element exposing (layers, show, container, topRight, leftAligned)
import Text
import Mouse
import Keyboard
import Window
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Time exposing (Time, every, millisecond)

import Game


-- The width and height of each piece on the board:
pieceSize = 10


-- Model:


type alias Model =
  { world : Game.World
  , running : Bool
  , generation : Int
  , cursor : (Int, Int)
  }


initialModel : Model
initialModel =
    { world = Game.initialWorld
    , running = False
    , generation = 0
    , cursor = (0, 0)
    }


model : Signal Model
model =
  Signal.foldp update initialModel actions


update action model =
  case action of
    Tick ->
      if model.running then
        { model | world <- Game.evolve model.world , generation <- model.generation + 1 }
      else
        model

    Click (x, y) ->
      { model | world <- Game.toggleCell model.world (x, y) }

    ToggleState ->
      { model | running <- not model.running }

    MoveCursor (x, y) ->
      { model | cursor <- (x, y) }


-- Actions:


updateInterval = (200 * millisecond)


type Action = Tick | Click (Int, Int) | ToggleState | MoveCursor (Int, Int)


adjustMouseCoordinates (w, h) (x, y) = 
  let
      x' = x - w // 2
      y' = 0 - y + h // 2
  in
      (x' // pieceSize, y' // pieceSize)


clicks : Signal (Int, Int)
clicks =
    -- Sample the mouse position on each click, then adjust the coordinates
    -- to match the game coordinate system.
    Signal.sampleOn Mouse.clicks Mouse.position
      |> Signal.map2 adjustMouseCoordinates Window.dimensions


spacePresses : Signal ()
spacePresses =
  -- `Keyboard.space` is a signal of Bool, where the value indicates whether the
  -- space key is pressed. We only want to fire when that value is true, so we
  -- filter out all False values.
  Signal.filter identity False Keyboard.space
    |> Signal.map (always ()) -- Drop the actual value, it's not interesting.


cursorPosition : Signal (Int, Int)
cursorPosition =
  Signal.map2 adjustMouseCoordinates Window.dimensions Mouse.position


actions : Signal Action
actions =
  Signal.mergeMany
    [ Signal.map Click clicks
    , Signal.map (always Tick) (every updateInterval)
    , Signal.map (always ToggleState) spacePresses
    , Signal.map MoveCursor cursorPosition
    ]


-- Rendering:


main = Signal.map2 view Window.dimensions model


renderCell (x, y) =
  let
      piece = Collage.filled Color.black (Collage.square pieceSize)
      x' = toFloat (x * pieceSize)
      y' = toFloat (y * pieceSize)
  in
    piece
      |> Collage.move (x', y')


renderCursor (x, y) =
  let
      cursor = Collage.filled Color.blue (Collage.square pieceSize)
      x' = toFloat (x * pieceSize)
      y' = toFloat (y * pieceSize)
  in
    cursor
      |> Collage.move (x', y')



view : (Int, Int) -> Model -> Html
view (w, h) model =
  let
      cellViews = List.map renderCell (Game.cells model.world)

      cursor = renderCursor model.cursor

      collage = Collage.collage w h (cellViews ++ [cursor])

      info =
          "Generation: " ++ toString model.generation
            |> Text.fromString
            |> leftAligned

      screen = layers
        [ collage
        , container w h topRight <| info
        ]
  in
      div [ style [ ("cursor", "none") ] ]
        [ Html.fromElement screen
        ]
