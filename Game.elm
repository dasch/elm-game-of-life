module Game where

import Set
import Patterns

type alias Cell = (Int, Int)
type alias World = Set.Set Cell

initialWorld : World
initialWorld =
    Set.empty
        |> insertPattern Patterns.blinker (0, 0)
        |> insertPattern Patterns.blinker (10, 0)
        |> insertPattern Patterns.glider (0, 10)
        |> insertPattern Patterns.toad (-10, 10)
        |> insertPattern Patterns.beacon (-30, 0)
        |> insertPattern Patterns.acorn (-30, -10)


evolve : World -> World
evolve world =
    Set.union (survivingCells world) (spawnedCells world)


insertPattern pattern (dx, dy) world =
    let
        move (x, y) = (x + dx, y + dy)
        cells = Set.fromList (List.map move pattern)
    in
        Set.union world cells


survivingCells world =
  let
      canSurvive cell = neighborCount world cell `List.member` [2, 3]
  in
      Set.filter canSurvive world


spawnedCells world =
  let
      canSpawn cell =
          neighborCount world cell == 3

      candidates =
        List.map neighbors (Set.toList world)
          |> List.foldl Set.union Set.empty
  in
      Set.filter canSpawn candidates


neighborCount world cell =
    let
        neighborCells = Set.intersect (neighbors cell) world
    in
        List.length (Set.toList neighborCells)

neighbors : Cell -> Set.Set Cell
neighbors (x, y) =
    Set.fromList
        [ (x - 1, y - 1)
        , (x - 1, y)
        , (x - 1, y + 1)
        , (x, y - 1)
        , (x, y + 1)
        , (x + 1, y - 1)
        , (x + 1, y)
        , (x + 1, y + 1)
        ]

