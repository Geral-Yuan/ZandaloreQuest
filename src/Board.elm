module Board exposing (Board, initBoard, sampleBoard)

import Data exposing (..)
import Message exposing (Msg(..))
import Time exposing (ZoneName(..))


type alias Board =
    { map : List Pos
    , obstacles : List Obstacle
    , enemies : List Enemy
    , heroes : List Hero
    , totalHeroNumber : Int
    , turn : Turn
    , cntEnemy : Int
    , cntTurret : Int
    , boardState : BoardState
    , critical : Int
    , moveable : List ( Pos, Dir )
    , attackable : List Pos
    , enemyAttackable : List Pos
    , skillable : List Pos
    , target : List Pos
    , item : List Item
    , timeTurn : Float
    , timeBoardState : Float
    , spawn : Int -- number of times group of enemies will be spawned
    , index : Int -- highest enemies index
    , pointPos : ( Float, Float )
    , coins : Int
    , level : Int
    }


initObstacles : Int -> List Obstacle
initObstacles k =
    -- need to change this
    case k of
        0 ->
            [ Obstacle MysteryBox ( 5, 5 ) (Gold 3)
            , Obstacle MysteryBox ( 5, 4 ) EnergyPotion
            , Obstacle MysteryBox ( 6, 5 ) HealthPotion
            ]

        1 ->
            List.map (\pos -> Obstacle Unbreakable pos NoItem) [ ( 2, 6 ), ( 5, 5 ), ( 6, 2 ) ]
                ++ [ Obstacle MysteryBox ( 4, 8 ) (Gold 3)
                   , Obstacle MysteryBox ( 8, 4 ) EnergyPotion
                   ]

        2 ->
            List.map (\pos -> Obstacle Unbreakable pos NoItem) [ ( 1, 9 ), ( 3, 7 ), ( 5, 5 ), ( 7, 3 ), ( 9, 1 ) ]
                ++ [ Obstacle MysteryBox ( 2, 8 ) (Gold 3)
                   , Obstacle MysteryBox ( 4, 6 ) EnergyPotion
                   , Obstacle MysteryBox ( 6, 4 ) HealthPotion
                   , Obstacle MysteryBox ( 8, 2 ) (Gold 3)
                   ]

        3 ->
            []

        _ ->
            [ Obstacle Unbreakable ( 5, 5 ) NoItem ]


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

        _ ->
            [ sampleEnemy Assassin ( 1, 9 ) 1
            , sampleEnemy Assassin ( 9, 5 ) 2
            , sampleEnemy Assassin ( 5, 1 ) 3
            ]


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

        _ ->
            2


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
    , moveable = []
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
    }


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
    , moveable = []
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
    }
