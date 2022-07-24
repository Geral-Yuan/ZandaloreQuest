module Data exposing (..)

import Svg.Attributes exposing (x2, y2)



-- Basic types


type GameMode
    = Castle
    | Shop
    | Dungeon
    | Dungeon2
    | BuyingItems
    | UpgradePage
    | HeroChoose
      -- | Starting
      -- | ClearLevel Int
      -- | Gameover Int
    | BoardGame
    | Logo
    | Tutorial Int
    | Dialog Task


type Task
    = MeetElder
    | FinishTutorial
    | GoToShop
    | Level Int
      -- | Beaten (to be added later)
    | GoToDungeon


type Scene
    = CastleScene
    | ShopScene
    | DungeonScene
    | Dungeon2Scene


type alias Pos =
    ( Int, Int )


type Critical
    = Less
    | None
    | Low
    | Medium
    | High


type Turn
    = PlayerTurn
    | TurretTurn
    | EnemyTurn


type BoardState
    = NoActions
    | EnemyAttack
    | TurretAttack
    | HeroAttack
    | HeroMoving
    | HeroHealth
    | HeroEnergy
    | Healing



-- | GettingAttacked


type Class
    = Warrior
    | Archer
    | Assassin
    | Healer
    | Mage
    | Engineer
    | Turret


type ObstacleType
    = MysteryBox
    | Unbreakable


type ItemType
    = HealthPotion
    | EnergyPotion
    | Gold Int
    | Buff
    | NoItem


type HeroState
    = Waiting
    | Attacking
    | Attacked Int
    | Moving
    | TakingHealth Int
    | TakingEnergy
    | GettingHealed Int


type alias Obstacle =
    { obstacleType : ObstacleType
    , pos : Pos
    , itemType : ItemType
    }


type alias Item =
    { itemType : ItemType
    , pos : Pos
    }


type alias Hero =
    { class : Class
    , pos : Pos
    , maxHealth : Int
    , health : Int
    , damage : Int
    , energy : Int
    , selected : Bool
    , state : HeroState
    , indexOnBoard : Int --give an index to the heroes on the board
    }


type alias Enemy =
    { class : Class
    , pos : Pos
    , maxHealth : Int
    , health : Int
    , damage : Int
    , steps : Int
    , done : Bool
    , state : HeroState
    , justAttack : Bool
    , indexOnBoard : Int --give an index to the enemies on the board
    }


type alias NPC =
    { scene : Scene
    , name : String
    , dialogue : List String
    , image : String
    , faceDir : Dir
    , position : ( Float, Float )
    , size : ( Float, Float )
    , beaten : Bool
    , talkRange : ( ( Float, Float ), ( Float, Float ) )
    }


type Dir
    = W
    | E
    | D
    | X
    | Z
    | A
    | Left
    | Right
    | Up
    | Down


type Side
    = Hostile
    | Friend



-- type RpgDir
--     = Left
--     | Right
--     | Up
--     | Down
-- Basic values


pixelWidth : Float
pixelWidth =
    2000


pixelHeight : Float
pixelHeight =
    1000


sideLen : Float
sideLen =
    70


halfWid : Float
halfWid =
    35 * sqrt 3


neighbour : List Pos
neighbour =
    [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]


subneighbour : List Pos
subneighbour =
    [ ( 2, 0 ), ( 1, 1 ), ( 0, 2 ), ( -1, 2 ), ( -2, 2 ), ( -2, 1 ), ( -2, 0 ), ( -1, -1 ), ( 0, -2 ), ( 1, -2 ), ( 2, -2 ), ( 2, -1 ) ]


subsubneighbour : List Pos
subsubneighbour =
    List.map (\y -> List.concatMap (\x -> [ vecAdd x y ]) neighbour) subneighbour
        |> unionList


map : Int -> List Pos
map level =
    case level of
        0 ->
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
                |> List.filter (\( x, y ) -> x + y >= 9 && x + y <= 11)

        _ ->
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


sampleEnemy : Class -> Pos -> Int -> Enemy
sampleEnemy class pos index =
    case class of
        Warrior ->
            Enemy Warrior pos 80 80 8 0 True Waiting False index

        Archer ->
            Enemy Archer pos 40 40 10 0 True Waiting False index

        Assassin ->
            Enemy Assassin pos 40 40 10 0 True Waiting False index

        Healer ->
            Enemy Healer pos 50 50 5 0 True Waiting False index

        _ ->
            Enemy Mage pos 50 50 6 0 True Waiting False index



-- Basic Functions


neighborToDir : Pos -> Dir
neighborToDir pos =
    if pos == ( -1, 0 ) then
        W

    else if pos == ( 0, -1 ) then
        E

    else if pos == ( 1, -1 ) then
        D

    else if pos == ( 1, 0 ) then
        X

    else if pos == ( 0, 1 ) then
        Z

    else
        A


extentPos : List Pos -> List Pos -> List Pos
extentPos posList relativePos =
    List.concat (List.map (\pos -> List.map (vecAdd pos) relativePos) posList)


sameline : Pos -> List Pos
sameline pos =
    List.map (\k -> vecScale k pos) (List.range 1 8)


listIntersection : List a -> List a -> List a
listIntersection list1 list2 =
    List.filter (\x -> List.member x list2) list1


intersectionList : List (List a) -> List a
intersectionList llist =
    case llist of
        [] ->
            []

        [ list ] ->
            list

        list1 :: (list2 :: rest) ->
            intersectionList (listIntersection list1 list2 :: rest)


listDifference : List a -> List a -> List a
listDifference list1 list2 =
    List.filter (\x -> not (List.member x list2)) list1


listUnion : List a -> List a -> List a
listUnion list1 list2 =
    let
        newElements =
            List.filter (\x -> not (List.member x list2)) list1
    in
    list2 ++ newElements


unionList : List (List a) -> List a
unionList list_of_list =
    case list_of_list of
        [] ->
            []

        [ list ] ->
            list

        list1 :: (list2 :: restlists) ->
            unionList (listUnion list1 list2 :: restlists)


pairRange : Int -> ( Int, Int ) -> List Pos
pairRange x ( y1, y2 ) =
    List.map (Tuple.pair x) (List.range y1 y2)


posToString : ( Float, Float ) -> String
posToString ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


vecAdd : Pos -> Pos -> Pos
vecAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


vecScale : Int -> Pos -> Pos
vecScale a ( x, y ) =
    ( a * x, a * y )


cartesianProduct : (a -> b -> c) -> List a -> List b -> List c
cartesianProduct f x y =
    List.concatMap (\x_ -> List.map (f x_) y) x


findPos : ( Int, Int ) -> ( Float, Float )
findPos ( row, column ) =
    ( pixelWidth / 2 + toFloat (row - column) * halfWid, toFloat (80 + (row + column - 6) * 105) )


findChosenHero : ( Float, Float ) -> Int
findChosenHero ( x, y ) =
    let
        row =
            if y > 100 && y < 400 then
                1

            else if y > 500 && y < 900 then
                2

            else
                0

        column =
            if x > 250 && x < 550 then
                1

            else if x > 850 && x < 1150 then
                2

            else if x > 1450 && x < 1750 then
                3

            else
                0
    in
    if row == 0 || column == 0 then
        0

    else
        (row - 1) * 3 + column


offsetHero : Hero -> Float
offsetHero hero =
    if hero.selected then
        50

    else
        0


offsetEnemy : Bool -> Float
offsetEnemy selected =
    if selected then
        50

    else
        0


findHexagon : ( Float, Float ) -> Int -> Maybe Pos
findHexagon targetPos level =
    List.head (List.filter (inHexagon targetPos) (map level))


inHexagon : ( Float, Float ) -> Pos -> Bool
inHexagon ( x, y ) pos =
    let
        ( cx, cy ) =
            findPos pos
    in
    abs (x - cx) < halfWid && abs (x - cx) + sqrt 3 * abs (y - cy) < sqrt 3 * sideLen


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


allSampleHeroes : List (Hero, Int)
allSampleHeroes =
    [ ( Hero Warrior ( 0, 0 ) 80 80 15 5 False Waiting 0, 1 )
        , ( Hero Archer ( 0, 0 ) 40 40 20 5 False Waiting 0, 2 )
        , ( Hero Assassin ( 0, 0 ) 40 40 20 6 False Waiting 0, 3 )
        , ( Hero Mage ( 0, 0 ) 50 50 12 3 False Waiting 0, 4 )
        , ( Hero Healer ( 0, 0 ) 50 50 5 5 False Waiting 0, 5 )
        , ( Hero Engineer ( 0, 0 ) 50 50 5 5 False Waiting 0, 6 )
        ]

        