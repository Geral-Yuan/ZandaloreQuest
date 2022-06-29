module View exposing (view)

import Board exposing (..)
import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, col, div)
import Html.Attributes as HtmlAttr
import List exposing (length)
import Message exposing (Msg(..))
import Model exposing (Model)
import ShortestPath exposing (shortestPath)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import ViewAllEnemy exposing (..)
import ViewAllHero exposing (..)
import ViewOthers exposing (..)


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "background" "grey"
        ]
        [ viewAll model
        ]


viewAll : Model -> Html Msg
viewAll model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)

        board =
            model.board
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
                ++ List.map viewHero model.heroes
                ++ List.map viewEnemy model.board.enemies
                ++ List.map viewCoordinate board.map
                ++ viewRoute model.board model.heroes ( 1, 9 ) ( 9, 1 )
                ++ List.map viewEnemy model.board.enemies
            )
         , endTurnButton
         , viewHeroInfo model
         ]
            ++ viewEnemyInformation model
        )


viewMap : Board -> List (Svg msg)
viewMap board =
    List.map (viewCell board) board.map


viewCell : Board -> Pos -> Svg msg
viewCell board ( row, column ) =
    if List.member ( row, column ) board.barrier then
        Svg.polygon
            [ SvgAttr.fill "black"
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
