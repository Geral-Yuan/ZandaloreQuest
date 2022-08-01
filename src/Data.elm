module Data exposing (..)

import ListOperation exposing (unionList)
import Svg.Attributes exposing (class, cy, x2, y2)
import Type exposing (..)



-- Basic types


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
            basicMap
                |> List.filter (\( x, y ) -> x + y >= 9 && x + y <= 11)

        3 ->
            (basicMap
                |> List.filter (\( x, y ) -> modBy 2 (x + y) == 0)
            )
                ++ [ ( 6, 1 ), ( 1, 8 ), ( 9, 2 ), ( 4, 9 ) ]

        4 ->
            basicMap
                |> List.filter (\( x, y ) -> not (List.member ( x, y ) hollow))

        5 ->
            (basicMap
                |> List.filter (\( x, y ) -> distance ( 5, 5 ) ( x, y ) /= 3)
            )
                ++ [ ( 2, 5 ), ( 5, 8 ), ( 8, 2 ) ]

        6 ->
            (basicMap
                |> List.filter (\( x, y ) -> x /= y && x + 2 * y /= 15 && 2 * x + y /= 15)
            )
                ++ [ ( 5, 5 ) ]

        _ ->
            basicMap


basicMap : List Pos
basicMap =
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


hollow : List Pos
hollow =
    [ ( 3, 4 )
    , ( 4, 3 )
    , ( 4, 4 )
    , ( 2, 7 )
    , ( 2, 6 )
    , ( 3, 6 )
    , ( 4, 8 )
    , ( 3, 8 )
    , ( 4, 7 )
    , ( 7, 6 )
    , ( 6, 7 )
    , ( 6, 6 )
    , ( 8, 3 )
    , ( 8, 4 )
    , ( 7, 4 )
    , ( 6, 2 )
    , ( 7, 2 )
    , ( 6, 3 )
    ]


rotateHexagon : Bool -> Pos -> Pos -> Pos
rotateHexagon clockwise ( cx, cy ) ( xi, yi ) =
    let
        deltaX =
            xi - cx

        deltaY =
            yi - cy

        deltaXY =
            deltaX + deltaY
    in
    if clockwise then
        ( cx - deltaY, cy + deltaXY )

    else
        ( cx + deltaXY, cy - deltaX )


rotateStuff : Bool -> Pos -> { a | pos : Pos } -> { a | pos : Pos }
rotateStuff clockwise ( cx, cy ) stuff =
    let
        ( xi, yi ) =
            stuff.pos

        deltaX =
            xi - cx

        deltaY =
            yi - cy

        deltaXY =
            deltaX + deltaY
    in
    if clockwise then
        { stuff | pos = ( cx - deltaY, cy + deltaXY ) }

    else
        { stuff | pos = ( cx + deltaXY, cy - deltaX ) }


sampleEnemy : Class -> Pos -> Int -> Enemy
sampleEnemy class pos index =
    case class of
        Warrior ->
            Enemy Warrior pos 80 80 8 0 True Waiting False index False 0

        Archer ->
            Enemy Archer pos 30 30 10 0 True Waiting False index False 0

        Assassin ->
            Enemy Assassin pos 35 35 10 0 True Waiting False index False 0

        Healer ->
            Enemy Healer pos 40 40 5 0 True Waiting False index False 0

        _ ->
            Enemy Mage pos 50 50 6 0 True Waiting False index False 0


initBoss : Enemy
initBoss =
    Enemy Turret ( 5, 5 ) 300 300 20 0 True Waiting False 1 True 1



-- Basic Functions


extentPos : List Pos -> List Pos -> List Pos
extentPos posList relativePos =
    List.concat (List.map (\pos -> List.map (vecAdd pos) relativePos) posList)


sameline : Pos -> List Pos
sameline pos =
    List.map (\k -> vecScale k pos) (List.range 1 8)


pairRange : Int -> ( Int, Int ) -> List Pos
pairRange x ( y1, y2 ) =
    List.map (Tuple.pair x) (List.range y1 y2)


posToString : ( Float, Float ) -> String
posToString ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


vecAdd : Pos -> Pos -> Pos
vecAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


vecAddFloat : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
vecAddFloat ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


vecScale : Int -> Pos -> Pos
vecScale a ( x, y ) =
    ( a * x, a * y )


cartesianProduct : (a -> b -> c) -> List a -> List b -> List c
cartesianProduct f x y =
    List.concatMap (\x_ -> List.map (f x_) y) x


findPos : Bool -> Int -> Float -> ( Int, Int ) -> ( Float, Float )
findPos rotating level time ( row, column ) =
    let
        fixedPos =
            findFixedPos ( row, column )
    in
    if rotating then
        let
            theta =
                pi / 3 * (1 - time)
        in
        case level of
            5 ->
                if distance ( 5, 5 ) ( row, column ) == 4 then
                    rotatePos (findFixedPos ( 5, 5 )) theta fixedPos

                else if distance ( 5, 5 ) ( row, column ) == 2 then
                    rotatePos (findFixedPos ( 5, 5 )) (0 - theta) fixedPos

                else
                    fixedPos

            _ ->
                let
                    ( newX, newY ) =
                        if distance ( 5, 5 ) ( row, column ) == 1 then
                            rotatePos (findFixedPos ( 5, 5 )) theta fixedPos

                        else if distance ( 2, 5 ) ( row, column ) == 1 then
                            rotatePos (findFixedPos ( 2, 5 )) (0 - theta) fixedPos

                        else if distance ( 2, 8 ) ( row, column ) == 1 then
                            rotatePos (findFixedPos ( 2, 8 )) theta fixedPos

                        else if distance ( 5, 8 ) ( row, column ) == 1 then
                            rotatePos (findFixedPos ( 5, 8 )) (0 - theta) fixedPos

                        else if distance ( 8, 5 ) ( row, column ) == 1 then
                            rotatePos (findFixedPos ( 8, 5 )) theta fixedPos

                        else if distance ( 8, 2 ) ( row, column ) == 1 then
                            rotatePos (findFixedPos ( 8, 2 )) (0 - theta) fixedPos

                        else if distance ( 5, 2 ) ( row, column ) == 1 then
                            rotatePos (findFixedPos ( 5, 2 )) theta fixedPos

                        else
                            fixedPos
                in
                if distance ( 5, 5 ) ( row, column ) > 1 then
                    rotatePos (findFixedPos ( 5, 5 )) (0 - theta) ( newX, newY )

                else
                    ( newX, newY )

    else
        fixedPos


findFixedPos : ( Int, Int ) -> ( Float, Float )
findFixedPos ( row, column ) =
    ( pixelWidth / 2 + toFloat (row - column) * halfWid, toFloat (80 + (row + column - 6) * 105) )


rotatePos : ( Float, Float ) -> Float -> ( Float, Float ) -> ( Float, Float )
rotatePos ( cx, cy ) theta ( ix, iy ) =
    let
        ( deltaX, deltaY ) =
            ( ix - cx, iy - cy )
    in
    ( cx + deltaX * cos theta - deltaY * sin theta, cy + deltaX * sin theta + deltaY * cos theta )


findChosenHero : ( Float, Float ) -> Int
findChosenHero ( x, y ) =
    let
        row =
            if y > 200 && y < 500 then
                1

            else if y > 525 && y < 825 then
                2

            else
                0

        column =
            if x > 400 && x < 700 then
                1

            else if x > 850 && x < 1150 then
                2

            else if x > 1300 && x < 1600 then
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
            findFixedPos pos
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


allSampleHeroes : List ( Hero, Int )
allSampleHeroes =
    [ ( Hero Warrior ( 0, 0 ) 80 80 15 5 False Waiting 0, 1 )
    , ( Hero Archer ( 0, 0 ) 30 30 20 5 False Waiting 0, 2 )
    , ( Hero Assassin ( 0, 0 ) 35 35 20 6 False Waiting 0, 3 )
    , ( Hero Mage ( 0, 0 ) 50 50 12 3 False Waiting 0, 4 )
    , ( Hero Healer ( 0, 0 ) 40 40 5 5 False Waiting 0, 5 )
    , ( Hero Engineer ( 0, 0 ) 30 30 5 5 False Waiting 0, 6 )
    ]


initialHeroes : List Hero
initialHeroes =
    [ Hero Warrior ( 0, 0 ) 80 80 15 5 False Waiting 1
    , Hero Archer ( 0, 0 ) 30 30 20 5 False Waiting 2
    ]


upgradeDamage : Class -> Int
upgradeDamage class =
    case class of
        Warrior ->
            2

        Archer ->
            4

        Assassin ->
            5

        Mage ->
            2

        Healer ->
            2

        Engineer ->
            2

        _ ->
            0


upgradeHealth : Class -> Int
upgradeHealth class =
    case class of
        Warrior ->
            20

        Archer ->
            10

        Assassin ->
            10

        Mage ->
            10

        Healer ->
            15

        Engineer ->
            10

        _ ->
            0


mode2Scene : GameMode -> Scene
mode2Scene mode =
    case mode of
        Castle ->
            CastleScene

        Shop ->
            ShopScene

        Dungeon ->
            DungeonScene

        _ ->
            Dungeon2Scene


scene2Mode : Scene -> GameMode
scene2Mode scene =
    case scene of
        CastleScene ->
            Castle

        ShopScene ->
            Shop

        DungeonScene ->
            Dungeon

        Dungeon2Scene ->
            Dungeon2


class2Index : Class -> Int
class2Index class =
    case class of
        Warrior ->
            1

        Archer ->
            2

        Assassin ->
            3

        Mage ->
            4

        Healer ->
            5

        _ ->
            6


index2Class : Int -> Class
index2Class index =
    case index of
        1 ->
            Warrior

        2 ->
            Archer

        3 ->
            Assassin

        4 ->
            Mage

        5 ->
            Healer

        _ ->
            Engineer
