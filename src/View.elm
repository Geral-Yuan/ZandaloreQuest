module View exposing (view)

import Board exposing (..)
import Data exposing (..)
import ShortestPath exposing (shortestPath)
import Debug exposing (toString)
import Html exposing (Html, col, div)
import Html.Attributes as HtmlAttr
import List exposing (length)
import Message exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import ViewInfo exposing (..)


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
        board = model.board
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
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewMap model.board 
            ++ List.map viewHero model.heroes 
            ++ List.map viewEnemy model.board.enemy
            ++ (List.map (viewCoordinate) board.map)
            ++ viewRoute model.board model.heroes (1,9) (9,1)
            )
        , endTurnButton
        , viewHeroInfo model
        
             
        ]


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

viewCoordinate :  Pos -> Svg msg
viewCoordinate  ( row, column ) =
    let
        (c_x, c_y) = findPos ( row, column )
        s_row = toString row
        s_column = toString column
    in
    
    Svg.text_
        [ SvgAttr.x (toString c_x)
        , SvgAttr.y (toString c_y)
        , SvgAttr.textAnchor "middle"
        , SvgAttr.dominantBaseline "middle"
        , SvgAttr.fill "grey"
        ]
        [
            Svg.text (s_row ++ " , " ++ s_column) 
        ]

{- use it to view the shortest path -}
viewRoute : Board -> List Hero  -> Pos -> Pos -> List (Svg msg)
viewRoute board hero_list begin end=
    let
       
        list_points = shortestPath board hero_list begin end
        
    in

    List.map (\x -> Svg.circle
                [ SvgAttr.cx (toString (Tuple.first x))
                , SvgAttr.cy (toString (Tuple.second x))
                , SvgAttr.r "5"
                ]
                []) (List.map findPos list_points)
detPoints : ( Float, Float ) -> String
detPoints ( x, y ) =
    String.concat
        (List.map posToString
            [ ( x, y - 70 )
            , ( x + halfWid, y - 35 )
            , ( x + halfWid, y + 35 )
            , ( x, y + 70 )
            , ( x - halfWid, y + 35 )
            , ( x - halfWid, y - 35 )
            ]
        )


viewHero : Hero -> Svg msg
viewHero hero =
    let
        ( x, y ) =
            findPos hero.pos
    in
    case hero.class of
        Warrior ->
            Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 35))
                , SvgAttr.y (toString (y - 35))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "../assets/image/WarriorGood.png"
                ]
                []

        Archer ->
            Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 35))
                , SvgAttr.y (toString (y - 35))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "../assets/image/ArcherGood.png"
                ]
                []

        Assassin ->
            Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 35))
                , SvgAttr.y (toString (y - 35))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "../assets/image/AssassinGood.png"
                ]
                []

        Mage ->
            Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 35))
                , SvgAttr.y (toString (y - 35))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "../assets/image/MageGood.png"
                ]
                []


viewEnemy : Enemy -> Svg Msg
viewEnemy enemy =
    let
        ( x, y ) =
            findPos enemy.pos
    in
    case enemy.class of
        Warrior ->
            Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 35))
                , SvgAttr.y (toString (y - 35))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "../assets/image/WarriorBad.png"
                ]
                []

        Archer ->
            Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 35))
                , SvgAttr.y (toString (y - 35))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "../assets/image/ArcherBad.png"
                ]
                []

        Assassin ->
            Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 35))
                , SvgAttr.y (toString (y - 35))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "../assets/image/AssassinBad.png"
                ]
                []

        Mage ->
            Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 35))
                , SvgAttr.y (toString (y - 35))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "../assets/image/MageBad.png"
                ]
                []
