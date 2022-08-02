module VectorOperation exposing (..)

import ListOperation exposing (unionList)
import Type exposing (Pos)


neighbour : List Pos
neighbour =
    [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]


subneighbour : List Pos
subneighbour =
    [ ( 2, 0 ), ( 1, 1 ), ( 0, 2 ), ( -1, 2 ), ( -2, 2 ), ( -2, 1 ), ( -2, 0 ), ( -1, -1 ), ( 0, -2 ), ( 1, -2 ), ( 2, -2 ), ( 2, -1 ) ]


subsubneighbour : List Pos
subsubneighbour =
    List.map (\x_ -> List.map (vecAdd x_) subneighbour) neighbour
        |> unionList


vecAdd : Pos -> Pos -> Pos
vecAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


vecAddFloat : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
vecAddFloat ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


vecScale : Int -> Pos -> Pos
vecScale a ( x, y ) =
    ( a * x, a * y )


distance : Pos -> Pos -> Int
distance ( x1, y1 ) ( x2, y2 ) =
    let
        maxDis =
            max (max (abs (x1 - x2)) (abs (y1 - y2))) (abs (x1 + y1 - x2 - y2))
    in
    abs (x1 - x2) + abs (y1 - y2) + abs (x1 + y1 - x2 - y2) - maxDis


leastdistance : List Pos -> Pos -> Maybe Int
leastdistance pos_list pos =
    List.minimum (List.map (distance pos) pos_list)


extentPos : List Pos -> List Pos -> List Pos
extentPos posList relativePos =
    List.concatMap (\pos -> List.map (vecAdd pos) relativePos) posList


sameline : Pos -> List Pos
sameline pos =
    List.map (\k -> vecScale k pos) (List.range 1 8)
