import Html exposing (..)
import Html.Attributes exposing (..)
import Graphics.Collage exposing (..)
import Color
import Set
import Time exposing (..)

import Game


updateInterval = (200 * millisecond)


clock : Signal Time
clock = Time.every updateInterval


world : Signal Game.World
world =
  Signal.foldp update Game.initialWorld clock


main = Signal.map view world


update t world = Game.evolve world

boardWidth = 1200
boardHeight = 800

pieceSize = 10

piece = filled Color.black (rect pieceSize pieceSize)

renderCell (x, y) = move (toFloat x * pieceSize, toFloat y * pieceSize) piece


view world =
  let
      cellViews = List.map renderCell (Set.toList world)
  in
      collage boardWidth boardHeight cellViews
