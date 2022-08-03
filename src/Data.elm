module Data exposing (allSampleHeroes, buttonHtmlAttr, class2Index, findChosenHero, findFixedPos, findHexagon, findPos, index2Class, initBoss, initialHeroes, mode2Scene, offsetEnemy, offsetHero, posToString, sampleEnemy, upgradeDamage, upgradeHealth)

{-| This file fills functions related to all the basic function used in the game.


# Functions

@docs allSampleHeroes, buttonHtmlAttr, class2Index, findChosenHero, findFixedPos, findHexagon, findPos, index2Class, initBoss, initialHeroes, mode2Scene, offsetEnemy, offsetHero, posToString, sampleEnemy, upgradeDamage, upgradeHealth

-}

import BoardMap exposing (map)
import Html exposing (Attribute)
import Html.Attributes as HtmlAttr
import Type exposing (Class(..), Enemy, GameMode(..), Hero, HeroState(..), Pos, Scene(..))
import VectorOperation exposing (distance)
import ViewConst exposing (halfWid, pixelWidth, sideLen)



-- Basic types


{-| This function will give sample of every class of enemy.
-}
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


{-| This function will initiate boss.
-}
initBoss : Enemy
initBoss =
    Enemy Turret ( 5, 5 ) 300 300 20 0 True Waiting False 1 True 1



-- Basic Functions


{-| This function will give the string form of the position
-}
posToString : ( Float, Float ) -> String
posToString ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


{-| This function will give the position of everything.
-}
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


{-| This function will find the fixed position.
-}
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


{-| This function will find the chosen hero during the choosing hero scene.
-}
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


{-| This function will give the offset of a hero when it is selected.
-}
offsetHero : Hero -> Float
offsetHero hero =
    if hero.selected then
        50

    else
        0


{-| This function will give the offset of an enemy when it is taking action.
-}
offsetEnemy : Bool -> Float
offsetEnemy selected =
    if selected then
        50

    else
        0


{-| This function will find set the hexagon cells position according to the level.
-}
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


{-| This function will give sample of every class of hero.
-}
allSampleHeroes : List ( Hero, Int )
allSampleHeroes =
    [ ( Hero Warrior ( 0, 0 ) 80 80 15 5 False Waiting 0, 1 )
    , ( Hero Archer ( 0, 0 ) 30 30 20 5 False Waiting 0, 2 )
    , ( Hero Assassin ( 0, 0 ) 35 35 20 6 False Waiting 0, 3 )
    , ( Hero Mage ( 0, 0 ) 50 50 12 3 False Waiting 0, 4 )
    , ( Hero Healer ( 0, 0 ) 40 40 5 5 False Waiting 0, 5 )
    , ( Hero Engineer ( 0, 0 ) 30 30 5 5 False Waiting 0, 6 )
    ]


{-| This function will give the initial heroes at the beginning of the game.
-}
initialHeroes : List Hero
initialHeroes =
    [ Hero Warrior ( 0, 0 ) 80 80 15 5 False Waiting 1
    , Hero Archer ( 0, 0 ) 30 30 20 5 False Waiting 2
    ]


{-| This function will determine how many damage each class will be upgraded per once.
-}
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


{-| This function will determine how many health each class will be upgraded per once.
-}
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


{-| This function will convert from model mode to model scene.
-}
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


{-| This function will convert from class to index
-}
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


{-| This function will convert the index to class.
-}
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


{-| This function will set the common Html Attribute for button.
-}
buttonHtmlAttr : List (Attribute msg)
buttonHtmlAttr =
    [ HtmlAttr.style "background" "transparent"
    , HtmlAttr.style "border" "transparent"
    , HtmlAttr.style "color" "rgb(61,43,31)"
    , HtmlAttr.style "position" "absolute"
    , HtmlAttr.style "text-align" "center"
    , HtmlAttr.style "font-family" "myfont"
    , HtmlAttr.style "font-weight" "bold"
    ]
