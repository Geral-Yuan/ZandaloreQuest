module Board exposing (Board, initBoard)

import Data exposing (..)
import Message exposing (Msg(..))


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
    , heroes : List Hero
    , turn : Turn
    , critical : Int
    , moveable : List Pos
    , attackable : List Pos
    , time : Float
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


inithero : Int -> List Hero
inithero k =
    case k of
        _ ->
            [ Hero Healer ( 6, 6 ) 60 15 5 5 False 1 -- for healer, damage is to heal
            , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
            , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
            ]


initBoard : Int -> Board
initBoard k =
    case k of
        _ ->
            Board map (initbarrier k) (initenemy k) (inithero k) HeroTurn 0 [] [] 0
