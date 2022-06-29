module ViewAllEnemy exposing (..)

import Board exposing (..)
import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, col, div)
import Html.Attributes as HtmlAttr exposing (style)
import List exposing (length)
import Message exposing (Msg(..))
import Model exposing (Model)
import ShortestPath exposing (shortestPath)
import Svg exposing (..)
import Svg.Attributes as SvgAttr


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


viewEnemyInformation : Model -> List (Html Msg)
viewEnemyInformation model =
    let
        enemy =
            { class = Data.Warrior
            , pos = ( 6, 6 )
            , health = 100
            , damage = 15
            , armour = 5
            , steps = 0
            , done = False
            , indexOnBoard = 1
            }
    in
    List.range 1 3
        |> List.map (getEnemy enemy model.board.enemies)
        |> List.map viewEnemyInfo


getEnemy : Enemy -> List Enemy -> Int -> Enemy
getEnemy defaultoutput enemy n =
    case enemy of
        [] ->
            defaultoutput

        x :: xs ->
            if n == 1 then
                x

            else
                getEnemy defaultoutput xs (n - 1)


viewEnemyInfo : Enemy -> Html Msg
viewEnemyInfo enemy =
    -- display health and energy
    div
        [ HtmlAttr.style "top" (toString (20 + (enemy.indexOnBoard - 1) * 120) ++ "px")
        , HtmlAttr.style "left" "-120px"
        , HtmlAttr.style "color" "black"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("Enemy" ++ toString enemy.indexOnBoard ++ ":" ++ toString enemy.health) ]
