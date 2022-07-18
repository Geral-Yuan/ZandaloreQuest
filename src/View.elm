module View exposing (view)

import Board exposing (Board)
import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import ViewAllEnemy exposing (..)
import ViewAllHero exposing (..)
import ViewChoose exposing (viewHeroChoose, viewShopChoose)
import ViewOthers exposing (..)
import ViewScenes exposing (..)


view : Model -> Html Msg
view model =
    let
        viewAll =
            case model.mode of
                Logo ->
                    viewScene0 model

                BoardGame ->
                    viewBoard model

                Castle ->
                    viewCastle model

                Shop ->
                    viewShop model

                HeroChoose ->
                    viewHeroChoose model

                BuyingItems ->
                    viewShopChoose model

                Dungeon ->
                    viewDungeon model

                Dungeon2 ->
                    viewDungeon2 model

                Tutorial k ->
                    viewTutorial k model
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "background" "black"
        ]
        [ viewAll
        ]


viewTutorial : Int -> Model -> Html Msg
viewTutorial k model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")

        -- , HtmlAttr.style "background" "grey"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ Svg.image
                [ SvgAttr.width (String.fromFloat pixelWidth)
                , SvgAttr.height (String.fromFloat pixelHeight)
                , SvgAttr.x "0"
                , SvgAttr.y "0"
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref ("./assets/image/Tutorial" ++ toString k ++ ".jpg")
                ]
                []
            ]
        , div
            [ HtmlAttr.style "width" "500px"
            , HtmlAttr.style "height" "20px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "1100px"
            , HtmlAttr.style "top" "0px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-size" "50px"
            ]
            [ text "Click enter to continue" ]
        ]


tutorialButton : Html Msg
tutorialButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "900px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "left" "20px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "170px"
        , onClick ViewTutorial
        ]
        [ text "How to play" ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "grey"
        ]
        ([ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewMap model.board
                ++ List.map viewCoordinate model.board.map
                ++ List.map viewMoveable model.board.moveable
                ++ List.map viewHeroImage model.board.heroes
                ++ List.map viewHeroFrame model.board.heroes
                ++ List.concat (List.map viewHeroCondition model.board.heroes)
                ++ List.concat (List.map viewHeroHealth model.board.heroes)
                ++ List.map (viewEnemyImage model.board) model.board.enemies
                ++ List.map (viewEnemyFrame model.board) model.board.enemies
                ++ List.concat (List.map (viewEnemyCondition model.board) model.board.enemies)
                ++ List.concat (List.map (viewEnemyHealth model.board) model.board.enemies)
                ++ List.map viewCrate model.board.obstacles
                ++ List.concatMap viewItem model.board.item
             --++ viewLines model.board
            )
         , endTurnButton
         , viewCritical model.board
         , viewBoardCoin model.board
         , viewLevel model.level
         , viewTurn model

         --  , viewClickPosition model
         --  , viewTips
         , tutorialButton
         ]
            ++ List.map viewHero model.board.heroes
            ++ List.concat (List.map viewHeroInfo model.board.heroes)
            ++ List.map viewEnemy model.board.enemies
            ++ List.concat (List.map (viewEnemyInfo model.board) model.board.enemies)
            ++ List.map animateHeroVisuals model.board.heroes
            ++ List.map animateEnemyVisuals model.board.enemies
        )


animateEnemyVisuals : Enemy -> Html Msg
animateEnemyVisuals enemy =
    let
        ( x, y ) =
            findPos enemy.pos
    in
    div
        [ HtmlAttr.style "left" (toString x ++ "px")
        , HtmlAttr.style "top" (toString (y - 80) ++ "px")
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "60px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ case enemy.state of
            Attacked k ->
                text ("-" ++ toString k)

            _ ->
                text ""
        ]


animateHeroVisuals : Hero -> Html Msg
animateHeroVisuals hero =
    let
        ( x, y ) =
            findPos hero.pos
    in
    div
        [ HtmlAttr.style "left" (toString (x + 40) ++ "px")
        , HtmlAttr.style "top" (toString (y - 80) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "60px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ case hero.state of
            Moving ->
                text "-2 Energy"

            TakingEnergy ->
                text "+2 Energy"

            TakingHealth h1 ->
                text ("+" ++ toString h1)

            GettingHealed h2 ->
                text ("+" ++ toString h2)

            Attacking ->
                text "-3 Energy"

            Attacked k ->
                text ("-" ++ toString k)

            _ ->
                text ""
        ]



-- view where the enermy archer can attack the hero (for debugging)
{-
   viewLines :  Board -> List (Svg msg)
   viewLines board  =
       let

           list_points =
               --List.concatMap (\x -> leastArcherPath x board) board.enemies
                getHeroesLines board
       in
       List.map
           (\x ->
               Svg.circle
                   [ SvgAttr.cx (toString (Tuple.first x))
                   , SvgAttr.cy (toString (Tuple.second x))
                   , SvgAttr.r "5"
                   ]
                   []
           )
           (List.map findPos list_points)
-}


viewMap : Board -> List (Svg Msg)
viewMap board =
    List.map (viewCell board) board.map


viewCell : Board -> Pos -> Svg Msg
viewCell board ( row, column ) =
    if List.member ( row, column ) (List.map .pos (List.filter (\obstacle -> obstacle.obstacleType == Unbreakable) board.obstacles)) then
        Svg.polygon
            [ SvgAttr.fill "black"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints (findPos ( row, column )))
            ]
            []

    else if List.member ( row, column ) board.target then
        let
            skilllist =
                listIntersection board.target board.skillable
        in
        if not (List.isEmpty skilllist) then
            Svg.polygon
                [ SvgAttr.fill "rgb(154,205,50)"
                , SvgAttr.stroke "blue"
                , SvgAttr.points (detPoints (findPos ( row, column )))
                ]
                []

        else
            Svg.polygon
                [ SvgAttr.fill "yellow"
                , SvgAttr.stroke "blue"
                , SvgAttr.points (detPoints (findPos ( row, column )))
                ]
                []

    else if List.member ( row, column ) (listUnion board.attackable board.skillable ++ board.enemyAttackable) then
        Svg.polygon
            [ SvgAttr.fill "rgb(173,216,230)"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints (findPos ( row, column )))
            ]
            []

    else
        Svg.polygon
            [ SvgAttr.fill "white"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints (findPos ( row, column )))
            ]
            []


viewTurn : Model -> Html Msg
viewTurn model =
    div
        [ HtmlAttr.style "left" "1690px"
        , HtmlAttr.style "top" "500px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ case model.board.turn of
            EnemyTurn ->
                text "Enemy Turn"

            _ ->
                text "Your Turn"
        ]


viewCrate : Obstacle -> Svg Msg
viewCrate obs =
    let
        ( x, y ) =
            findPos obs.pos
    in
    if obs.obstacleType == MysteryBox then
        Svg.image
            [ SvgAttr.width "80"
            , SvgAttr.height "80"
            , SvgAttr.x (toString (x - 40))
            , SvgAttr.y (toString (y - 40))
            , SvgAttr.preserveAspectRatio "none"
            , SvgAttr.xlinkHref "./assets/image/Crate.png"
            ]
            []

    else
        -- TODO: if possible, return nothing
        Svg.rect [] []


viewItem : Item -> List (Svg Msg)
viewItem item =
    let
        ( x, y ) =
            findPos item.pos

        itemtype =
            toString item.itemType
    in
    case item.itemType of
        Gold _ ->
            [ Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 40))
                , SvgAttr.y (toString (y - 40))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "./assets/image/Gold.png"
                ]
                []
            ]

        NoItem ->
            []

        _ ->
            [ Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 40))
                , SvgAttr.y (toString (y - 40))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref ("./assets/image/" ++ itemtype ++ ".png")
                ]
                []
            ]
