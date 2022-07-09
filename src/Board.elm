module Board exposing (Board, initBoard, sampleBoard)

import Data exposing (..)
import Message exposing (Msg(..))


type alias Board =
    { map : List Pos
    , obstacles : List Obstacle
    , enemies : List Enemy
    , heroes : List Hero
    , turn : Turn
    , critical : Int
    , moveable : List ( Pos, Dir )
    , attackable : List Pos
    , target : List Pos
    , item : List Item
    , time : Float
    , spawn : Int -- number of times group of enemies will be spawned
    , index : Int -- highest enemies index
    , pointPos : ( Float, Float )
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
            [ Enemy Archer ( 3, 3 ) 100 15 5 0 True 1
            , Enemy Mage ( 1, 8 ) 100 9 1 0 True 2
            , Enemy Warrior ( 5, 2 ) 100 10 5 0 True 3
            ]


inithero : List Hero -> Int -> List Hero
inithero heroes k =
    List.map (initPosition k) heroes


initPosition : Int -> Hero -> Hero
initPosition k hero =
    case k of
        _ ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 6, 6 ) }

                2 ->
                    { hero | pos = ( 5, 8 ) }

                _ ->
                    { hero | pos = ( 8, 5 ) }


initBoard : List Hero -> Int -> Board
initBoard heroes k =
    case k of
        _ ->
            Board map (initObstacles k) (initenemy k) (inithero heroes k) HeroTurn 0 [] [] [] [] 0 1 3 ( 0, 0 )


sampleBoard : Board
sampleBoard =
    Board [] [] [] [] HeroTurn 0 [] [] [] [] 0 0 0 ( 0, 0 )
