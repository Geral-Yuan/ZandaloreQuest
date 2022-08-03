module UpdateMap exposing (updateMap)

{-| This file fills functions related to updating board game map.


# Function

@docs updateMap

-}

import BoardMap exposing (rotateStuff)
import Type exposing (Board, Pos)
import VectorOperation exposing (distance)


{-| This function will update map shape according to the level and rotating state.
-}
updateMap : Int -> Board -> Board
updateMap level board =
    case level of
        5 ->
            { board | mapRotating = ( True, 0 ) }
                |> rotate ( 5, 5 ) 4 False
                |> rotate ( 5, 5 ) 2 True

        6 ->
            { board | mapRotating = ( True, 0 ) }
                |> rotate ( 5, 5 ) 4 True
                |> rotate ( 5, 5 ) 3 True
                |> rotate ( 5, 5 ) 2 True
                |> rotate ( 5, 5 ) 1 False
                |> rotate ( 2, 5 ) 1 True
                |> rotate ( 2, 8 ) 1 False
                |> rotate ( 5, 8 ) 1 True
                |> rotate ( 8, 5 ) 1 False
                |> rotate ( 8, 2 ) 1 True
                |> rotate ( 5, 2 ) 1 False

        _ ->
            board


rotate : Pos -> Int -> Bool -> Board -> Board
rotate center dis clockwise board =
    let
        ( targetHeroes, restHeroes ) =
            List.partition (\hero -> distance center hero.pos == dis) board.heroes

        ( targetEnemies, restEnemies ) =
            List.partition (\hero -> distance center hero.pos == dis) board.enemies

        ( targetObstacles, restObstacles ) =
            List.partition (\hero -> distance center hero.pos == dis) board.obstacles

        ( targetItems, restItems ) =
            List.partition (\hero -> distance center hero.pos == dis) board.item
    in
    { board
        | heroes = List.map (rotateStuff clockwise center) targetHeroes ++ restHeroes
        , enemies = List.map (rotateStuff clockwise center) targetEnemies ++ restEnemies
        , obstacles = List.map (rotateStuff clockwise center) targetObstacles ++ restObstacles
        , item = List.map (rotateStuff clockwise center) targetItems ++ restItems
    }
