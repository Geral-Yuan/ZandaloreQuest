module View exposing (view)

import Board exposing (Board)
import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Message exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import ViewAllEnemy exposing (..)
import ViewAllHero exposing (..)
import ViewChoose exposing (viewHeroChoose)
import ViewOthers exposing (..)
import ViewScenes exposing (..)


view : Model -> Html Msg
view model =
    let
        viewAll =
            case model.mode of
                Logo ->
                   viewScene0 model

                BoardGame _ ->
                    viewBoard1 model

                Castle ->
                    viewCastle model

                Shop ->
                    viewShop model

                HeroChoose _ ->
                    viewHeroChoose model
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


viewBoard1 : Model -> Html Msg
viewBoard1 model =
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
                ++ List.map viewHero model.board.heroes
                ++ List.map viewEnemy model.board.enemies
                ++ List.map viewCoordinate model.board.map
                ++ List.map viewMoveable model.board.moveable
                ++ List.map viewHeroInfo1 model.board.heroes
                ++ List.map viewHeroInfo2 model.board.heroes
                ++ List.map viewCrate model.board.obstacles
                ++ List.map viewItem model.board.item
             --++ viewLines model.board
            )
         , endTurnButton
         , viewCritical model.board
         , viewBoardCoin model.board
         , viewClickPosition model
         , viewTips
         ]
            ++ List.map viewHeroInfo3 model.board.heroes
            ++ List.map viewHeroInfo4 model.board.heroes
            ++ viewEnemyInformation (List.sortBy .indexOnBoard model.board.enemies) 1
        )



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


viewClickPosition : Model -> Html Msg
viewClickPosition model =
    let
        ( x, y ) =
            model.board.pointPos
    in
    div
        [ HtmlAttr.style "bottom" "30px"
        , HtmlAttr.style "left" "0px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("( " ++ toString (Basics.round x) ++ " ," ++ toString (Basics.round y) ++ " )") ]



-- Just for tips now. Later I will delete it


viewTips : Html Msg
viewTips =
    div
        [ HtmlAttr.style "bottom" "150px"
        , HtmlAttr.style "left" "0px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Tips: Hero's energy for attack and motion!" ]


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
        Svg.polygon
            [ SvgAttr.fill "yellow"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints (findPos ( row, column )))
            ]
            []

    else if List.member ( row, column ) board.attackable then
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


viewItem : Item -> Svg Msg
viewItem item =
    let
        ( x, y ) =
            findPos item.pos

        itype =
            toString item.itemType
    in
    Svg.image
        [ SvgAttr.width "80"
        , SvgAttr.height "80"
        , SvgAttr.x (toString (x - 40))
        , SvgAttr.y (toString (y - 40))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref ("./assets/image/" ++ itype ++ ".png")
        ]
        []
