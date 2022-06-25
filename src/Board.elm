module Board exposing (..)

import Data exposing (..)


pairRange : Int -> ( Int, Int ) -> List Pos
pairRange x ( y1, y2 ) =
    List.map (Tuple.pair x) (List.range y1 y2)


map : List Pos
map =
    pairRange 1 ( 6, 8 )
        ++ pairRange 2 ( 4, 9 )
        ++ pairRange 3 ( 3, 9 )
        ++ pairRange 4 ( 2, 9 )
        ++ pairRange 5 ( 2, 8 )
        ++ pairRange 6 ( 1, 8 )
        ++ pairRange 7 ( 1, 7 )
        ++ pairRange 8 ( 1, 6 )
        ++ pairRange 9 ( 2, 4 )


type alias Board =
    { map : List Pos
    , barrier : List Pos
    , enemy : List Enemy
    }


initbarrier : Int -> List Pos
initbarrier k =
    case k of
        _ ->
            [ ( 5, 5 ) ]


initenemy : Int -> List Enemy
initenemy k =
    case k of
        _ ->
            [ Enemy ( 3, 3 ) 100 10 5 ]


initBoard : Int -> Board
initBoard k =
    case k of
        _ ->
            Board map (initbarrier k) (initenemy k)
