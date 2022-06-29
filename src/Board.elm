module Board exposing (..)

import Data exposing (..)


pairRange : Int -> ( Int, Int ) -> List Pos
pairRange x ( y1, y2 ) =
    List.map (Tuple.pair x) (List.range y1 y2)


map : List Pos
map =
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


type alias Board =
    { map : List Pos
    , barrier : List Pos
    , enemies : List Enemy
    , turn : Turn
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
            [ Enemy Warrior ( 3, 3 ) 100 10 5 0 True 1
            , Enemy Warrior ( 1, 8 ) 100 10 5 0 True 2
            , Enemy Warrior ( 5, 2 ) 100 10 5 0 True 3
            ]


initBoard : Int -> Board
initBoard k =
    case k of
        _ ->
            Board map (initbarrier k) (initenemy k) HeroTurn
