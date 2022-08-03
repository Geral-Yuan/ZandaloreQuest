module BoardMap exposing (map, rotateStuff)

{-| This file fills functions related to board map or shape.


# Functions

@docs map, rotateStuff

-}

import Type exposing (Pos)
import VectorOperation exposing (distance)


{-| This function will give the shape of map according to the level.
-}
map : Int -> List Pos
map level =
    case level of
        0 ->
            basicMap
                |> List.filter (\( x, y ) -> x + y >= 9 && x + y <= 11)

        3 ->
            (basicMap
                |> List.filter (\( x, y ) -> modBy 2 (x + y) == 0)
            )
                ++ [ ( 6, 1 ), ( 1, 8 ), ( 9, 2 ), ( 4, 9 ) ]

        4 ->
            basicMap
                |> List.filter (\( x, y ) -> not (List.member ( x, y ) hollow))

        5 ->
            (basicMap
                |> List.filter (\( x, y ) -> distance ( 5, 5 ) ( x, y ) /= 3)
            )
                ++ [ ( 2, 5 ), ( 5, 8 ), ( 8, 2 ) ]

        6 ->
            (basicMap
                |> List.filter (\( x, y ) -> x /= y && x + 2 * y /= 15 && 2 * x + y /= 15)
            )
                ++ [ ( 5, 5 ) ]

        _ ->
            basicMap


basicMap : List Pos
basicMap =
    List.concat
        (List.map2 pairRange
            (List.range 1 9)
            [ ( 5, 9 )
            , ( 4, 9 )
            , ( 3, 9 )
            , ( 2, 9 )
            , ( 1, 9 )
            , ( 1, 8 )
            , ( 1, 7 )
            , ( 1, 6 )
            , ( 1, 5 )
            ]
        )


hollow : List Pos
hollow =
    let
        first_part =
            [ ( 3, 4 )
            , ( 4, 3 )
            , ( 4, 4 )
            , ( 2, 7 )
            , ( 2, 6 )
            , ( 3, 6 )
            , ( 4, 8 )
            , ( 3, 8 )
            , ( 4, 7 )
            ]

        second_part =
            [ ( 7, 6 )
            , ( 6, 7 )
            , ( 6, 6 )
            , ( 8, 3 )
            , ( 8, 4 )
            , ( 7, 4 )
            , ( 6, 2 )
            , ( 7, 2 )
            , ( 6, 3 )
            ]
    in
    first_part ++ second_part


{-| This function will rotate the position of everything on the board.
-}
rotateStuff : Bool -> Pos -> { a | pos : Pos } -> { a | pos : Pos }
rotateStuff clockwise ( cx, cy ) stuff =
    let
        ( xi, yi ) =
            stuff.pos

        deltaX =
            xi - cx

        deltaY =
            yi - cy

        deltaXY =
            deltaX + deltaY
    in
    if clockwise then
        { stuff | pos = ( cx - deltaY, cy + deltaXY ) }

    else
        { stuff | pos = ( cx + deltaXY, cy - deltaX ) }


pairRange : Int -> ( Int, Int ) -> List Pos
pairRange x ( y1, y2 ) =
    List.map (Tuple.pair x) (List.range y1 y2)
