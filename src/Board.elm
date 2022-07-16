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
    , skillable : List Pos
    , target : List Pos
    , item : List Item
    , time : Float
    , spawn : Int -- number of times group of enemies will be spawned
    , index : Int -- highest enemies index
    , pointPos : ( Float, Float )
    , coins : Int
    }


initObstacles : Int -> List Obstacle
initObstacles k =
    -- need to change this
    case k of
        1 ->
            List.map (\pos -> Obstacle Unbreakable pos NoItem) [ ( 2, 6 ), ( 5, 5 ), ( 6, 2 ) ]
                ++ [ Obstacle MysteryBox ( 4, 8 ) (Gold 3)
                   , Obstacle MysteryBox ( 8, 4 ) EnergyPotion
                   ]

        _ ->
            List.map (\pos -> Obstacle Unbreakable pos NoItem) [ ( 1, 9 ), ( 3, 7 ), ( 5, 5 ), ( 7, 3 ), ( 9, 1 ) ]
                ++ [ Obstacle MysteryBox ( 2, 8 ) (Gold 3)
                   , Obstacle MysteryBox ( 4, 6 ) EnergyPotion
                   , Obstacle MysteryBox ( 6, 4 ) HealthPotion
                   , Obstacle MysteryBox ( 8, 2 ) (Gold 3)
                   ]


initenemy : Int -> List Enemy
initenemy k =
    case k of
        1 ->
            [ sampleEnemy Archer ( 3, 3 ) 1
            , sampleEnemy Mage ( 1, 8 ) 2
            , sampleEnemy Warrior ( 5, 2 ) 3
            ]

        _ ->
            [ sampleEnemy Archer ( 1, 5 ) 1
            , sampleEnemy Mage ( 3, 3 ) 2
            , sampleEnemy Warrior ( 5, 1 ) 3
            ]


inithero : List Hero -> Int -> List Hero
inithero heroes k =
    List.map (initPosition k) heroes


initPosition : Int -> Hero -> Hero
initPosition k hero =
    case k of
        1 ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 6, 6 ) }

                2 ->
                    { hero | pos = ( 5, 8 ) }

                _ ->
                    { hero | pos = ( 8, 5 ) }

        _ ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 5, 9 ) }

                2 ->
                    { hero | pos = ( 7, 7 ) }

                _ ->
                    { hero | pos = ( 9, 5 ) }


spawnTimes : Int -> Int
spawnTimes k =
    case k of
        1 ->
            0

        _ ->
            1


initBoard : List Hero -> Int -> Board
initBoard heroes k =
    Board map (initObstacles k) (initenemy k) (inithero heroes k) PlayerTurn 0 [] [] [] [] [] 0 (spawnTimes k) (List.length (initenemy k)) ( 0, 0 ) 0


sampleBoard : Board
sampleBoard =
    Board [] [] [] [] PlayerTurn 0 [] [] [] [] [] 0 0 0 ( 0, 0 ) 0
