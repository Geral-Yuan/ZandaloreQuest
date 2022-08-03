module Board exposing (initBoard, sampleBoard)

{-| This file fills functions related to board.


# Functions

@docs initBoard, sampleBoard

-}

import BoardMap exposing (map)
import Data exposing (initBoss, sampleEnemy)
import Message exposing (Msg(..))
import Type exposing (Board, BoardState(..), Class(..), Enemy, FailToDo(..), Hero, ItemType(..), Obstacle, ObstacleType(..), Turn(..))


initObstacles : Int -> List Obstacle
initObstacles k =
    -- need to change this
    case k of
        0 ->
            [ Obstacle MysteryBox ( 5, 5 ) (Gold 5)
            , Obstacle MysteryBox ( 5, 4 ) EnergyPotion
            , Obstacle MysteryBox ( 6, 5 ) HealthPotion
            ]

        1 ->
            List.map (\pos -> Obstacle Unbreakable pos NoItem) [ ( 2, 6 ), ( 5, 5 ), ( 6, 2 ) ]
                ++ [ Obstacle MysteryBox ( 4, 8 ) (Gold 5)
                   , Obstacle MysteryBox ( 8, 4 ) EnergyPotion
                   ]

        2 ->
            List.map (\pos -> Obstacle Unbreakable pos NoItem) [ ( 1, 9 ), ( 3, 7 ), ( 5, 5 ), ( 7, 3 ), ( 9, 1 ) ]
                ++ [ Obstacle MysteryBox ( 2, 8 ) (Gold 5)
                   , Obstacle MysteryBox ( 4, 6 ) EnergyPotion
                   , Obstacle MysteryBox ( 6, 4 ) HealthPotion
                   , Obstacle MysteryBox ( 8, 2 ) (Gold 5)
                   ]

        3 ->
            []

        4 ->
            [ Obstacle Unbreakable ( 5, 5 ) NoItem ]

        5 ->
            List.map (\pos -> Obstacle Unbreakable pos NoItem) [ ( 4, 5 ), ( 5, 6 ), ( 6, 4 ) ]

        _ ->
            List.map (\pos -> Obstacle Unbreakable pos NoItem) [ ( 2, 5 ), ( 2, 8 ), ( 5, 8 ), ( 8, 5 ), ( 8, 2 ), ( 5, 2 ) ]


initenemy : Int -> List Enemy
initenemy k =
    case k of
        0 ->
            [ sampleEnemy Archer ( 8, 2 ) 1 ]

        1 ->
            [ sampleEnemy Healer ( 3, 3 ) 1
            , sampleEnemy Mage ( 1, 8 ) 2
            , sampleEnemy Warrior ( 5, 2 ) 3
            ]

        2 ->
            [ sampleEnemy Archer ( 1, 5 ) 1
            , sampleEnemy Mage ( 3, 3 ) 2
            , sampleEnemy Warrior ( 5, 1 ) 3
            ]

        3 ->
            [ sampleEnemy Archer ( 2, 4 ) 1
            , sampleEnemy Archer ( 3, 3 ) 2
            , sampleEnemy Archer ( 4, 2 ) 3
            ]

        4 ->
            [ sampleEnemy Assassin ( 1, 9 ) 1
            , sampleEnemy Assassin ( 9, 5 ) 2
            , sampleEnemy Assassin ( 5, 1 ) 3
            ]

        5 ->
            [ sampleEnemy Archer ( 1, 9 ) 1
            , sampleEnemy Archer ( 9, 5 ) 2
            , sampleEnemy Archer ( 5, 1 ) 3
            ]

        _ ->
            [ initBoss ]


inithero : List Hero -> Int -> List Hero
inithero heroes k =
    List.map (initPosition k) heroes


initPosition : Int -> Hero -> Hero
initPosition k hero =
    case k of
        0 ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 2, 8 ) }

                2 ->
                    { hero | pos = ( 2, 7 ) }

                _ ->
                    { hero | pos = ( 3, 8 ) }

        1 ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 6, 6 ) }

                2 ->
                    { hero | pos = ( 5, 8 ) }

                _ ->
                    { hero | pos = ( 8, 5 ) }

        2 ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 5, 9 ) }

                2 ->
                    { hero | pos = ( 7, 7 ) }

                _ ->
                    { hero | pos = ( 9, 5 ) }

        3 ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 6, 8 ) }

                2 ->
                    { hero | pos = ( 7, 7 ) }

                _ ->
                    { hero | pos = ( 8, 6 ) }

        4 ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 1, 5 ) }

                2 ->
                    { hero | pos = ( 5, 9 ) }

                _ ->
                    { hero | pos = ( 9, 1 ) }

        5 ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 3, 7 ) }

                2 ->
                    { hero | pos = ( 7, 5 ) }

                _ ->
                    { hero | pos = ( 5, 3 ) }

        _ ->
            case hero.indexOnBoard of
                1 ->
                    { hero | pos = ( 1, 5 ) }

                2 ->
                    { hero | pos = ( 5, 9 ) }

                _ ->
                    { hero | pos = ( 9, 1 ) }


spawnTimes : Int -> Int
spawnTimes k =
    case k of
        0 ->
            0

        1 ->
            0

        2 ->
            1

        6 ->
            0

        _ ->
            2


{-| This function will return the initial state of Board.
-}
initBoard : List Hero -> Int -> Board
initBoard heroes k =
    { map = map k
    , obstacles = initObstacles k
    , enemies = initenemy k
    , heroes = inithero heroes k
    , totalHeroNumber = 3
    , turn = PlayerTurn
    , cntEnemy = 0
    , cntTurret = 0
    , boardState = NoActions
    , critical = 0
    , attackable = []
    , enemyAttackable = []
    , skillable = []
    , target = []
    , item = []
    , timeTurn = 0
    , timeBoardState = 0
    , spawn = spawnTimes k
    , index = List.length (initenemy k)
    , pointPos = ( 0, 0 )
    , coins = 0
    , level = k
    , mapRotating = ( False, 0 )
    , popUpHint = ( Noop, 0 )
    , hintOn = False
    }


{-| This function will provide a sample board.
-}
sampleBoard : Board
sampleBoard =
    { map = []
    , obstacles = []
    , enemies = []
    , heroes = []
    , totalHeroNumber = 0
    , turn = PlayerTurn
    , cntEnemy = 0
    , cntTurret = 0
    , boardState = NoActions
    , critical = 0
    , attackable = []
    , enemyAttackable = []
    , skillable = []
    , target = []
    , item = []
    , timeTurn = 0
    , timeBoardState = 0
    , spawn = 0
    , index = 0
    , pointPos = ( 0, 0 )
    , coins = 0
    , level = 0
    , mapRotating = ( False, 0 )
    , popUpHint = ( Noop, 0 )
    , hintOn = False
    }
