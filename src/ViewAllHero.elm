module ViewAllHero exposing (..)

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


viewHeroInfo : Model -> Html Msg
viewHeroInfo model =
    -- display information of the selected hero
    let
        sample_hero =
            Hero Warrior ( 0, 0 ) 0 0 0 0 False 0

        hero =
            List.filter .selected model.heroes
                |> List.head
                |> Maybe.withDefault sample_hero
    in
    if hero.selected then
        div
            [ HtmlAttr.style "top" "50px"
            , HtmlAttr.style "left" "1050px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
            , HtmlAttr.style "font-size" "50px"
            , HtmlAttr.style "font-weight" "bold"
            , HtmlAttr.style "text-align" "center"
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "position" "absolute"
            ]
            [ text
                (toString hero.class
                    ++ ("\nHealth: " ++ toString hero.health)
                    ++ ("\nDamage: " ++ toString hero.damage)
                    ++ ("\nArmour: " ++ toString hero.armour)
                    ++ ("\nEnergy: " ++ toString hero.energy)
                )
            ]

    else
        div [] []


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
