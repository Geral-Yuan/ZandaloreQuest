module VectorOperation exposing (distance, extentPos, leastdistance, neighbour, sameline, subneighbour, subsubneighbour, vecAdd, vecAddFloat, vecScale)

{-| This file fills functions related to common used vector operation.


# Function

@docs distance, extentPos, leastdistance, neighbour, sameline, subneighbour, subsubneighbour, vecAdd, vecAddFloat, vecScale

-}

import ListOperation exposing (unionList)
import Type exposing (Pos)


{-| This function will return list of positions that is one step away.
-}
neighbour : List Pos
neighbour =
    [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]


{-| This function will return list of positions that is two steps away.
-}
subneighbour : List Pos
subneighbour =
    [ ( 2, 0 ), ( 1, 1 ), ( 0, 2 ), ( -1, 2 ), ( -2, 2 ), ( -2, 1 ), ( -2, 0 ), ( -1, -1 ), ( 0, -2 ), ( 1, -2 ), ( 2, -2 ), ( 2, -1 ) ]

{-| This function will return list of positions that is in three steps
-}
subsubneighbour : List Pos
subsubneighbour =
    List.map (\x_ -> List.map (vecAdd x_) subneighbour) neighbour
        |> unionList


{-| This function will return the sum of two vectors.
-}
vecAdd : Pos -> Pos -> Pos
vecAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| This function will return the sum of two vectors in float type.
-}
vecAddFloat : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
vecAddFloat ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| This function will return the scale `a` of the vector.
-}
vecScale : Int -> Pos -> Pos
vecScale a ( x, y ) =
    ( a * x, a * y )


{-| This function will return the distance between two positions.
-}
distance : Pos -> Pos -> Int
distance ( x1, y1 ) ( x2, y2 ) =
    let
        maxDis =
            max (max (abs (x1 - x2)) (abs (y1 - y2))) (abs (x1 + y1 - x2 - y2))
    in
    abs (x1 - x2) + abs (y1 - y2) + abs (x1 + y1 - x2 - y2) - maxDis


{-| This function will return the least distance between a list of positions and another position.
-}
leastdistance : List Pos -> Pos -> Maybe Int
leastdistance pos_list pos =
    List.minimum (List.map (distance pos) pos_list)


{-| This function will return extent positions of `posList` shifted by `relativePos`
-}
extentPos : List Pos -> List Pos -> List Pos
extentPos posList relativePos =
    List.concatMap (\pos -> List.map (vecAdd pos) relativePos) posList


{-| This function will return a list of positions that is in the same line with `pos`
-}
sameline : Pos -> List Pos
sameline pos =
    List.map (\k -> vecScale k pos) (List.range 1 8)
