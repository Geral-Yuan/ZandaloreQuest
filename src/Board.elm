module Board exposing (Board, initBoard)

import Data exposing (..)
import Message exposing (Msg(..))


type alias Board =
    { map : List Pos
    , obstacles : List Obstacle
    , enemies : List Enemy
    , heroes : List Hero
    , turn : Turn
    , critical : Int
    , moveable : List Pos
    , attackable : List Pos
    , item : List Item
    , time : Float
    }


initObstacles : Int -> List Obstacle
initObstacles k =
    -- need to change this
    case k of
        _ ->
            [ Obstacle Unbreakable ( 5, 5 ) NoItem
            , Obstacle Unbreakable ( 2, 6 ) NoItem
            , Obstacle Unbreakable ( 6, 2 ) NoItem
            , Obstacle MysteryBox ( 4, 8 ) HealthPotion
            , Obstacle MysteryBox ( 8, 4 ) HealthPotion
            ]


initenemy : Int -> List Enemy
initenemy k =
    case k of
        _ ->
            [ Enemy Archer ( 3, 3 ) 100 10 5 0 True 1
            , Enemy Warrior ( 1, 8 ) 100 10 5 0 True 2
            , Enemy Warrior ( 5, 2 ) 100 10 5 0 True 3
            ]


inithero : Int -> List Hero
inithero k =
    case k of
        _ ->
            [ Hero Mage ( 6, 6 ) 50 15 5 3 False 1
            , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
            , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
            ]


initBoard : Int -> Board
initBoard k =
    case k of
        _ ->
            Board map (initObstacles k) (initenemy k) (inithero k) HeroTurn 0 [] [] [] 0
