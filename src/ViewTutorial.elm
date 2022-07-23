module ViewTutorial exposing (..)

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
import ViewChoose exposing (viewHeroChoose)
import ViewOthers exposing (..)
import ViewScenes exposing (..)
import ViewShop exposing (viewShopChoose)


viewTutorialScene : Int -> Model -> Html Msg
viewTutorialScene k model =
    case k of
        1 ->
            viewTutorial1 model

        2 ->
            viewTutorial2 model

        _ ->
            viewTutorial3 model


viewTutorial1 : Model -> Html Msg
viewTutorial1 model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ shapeHelper 145 430 15 15 "blue"
            , shapeHelper 450 440 1555 15 "blue"
            , shapeHelper 100 100 950 450 "blue"
            ]
        , dialogHelper 600 20 20 170 50 "blue" "Enemies Information (Red)"
        , dialogHelper 400 20 1150 50 50 "blue" "Your heroes' Information (Blue)"
        , dialogHelper 600 20 950 700 50 "blue" "Brown obstacle: mystery box that drops gold and potions (black means unbreakable)"
        , dialogHelper 600 20 20 900 50 "blue" "Click enter to continue"
        ]


viewTutorial2 : Model -> Html Msg
viewTutorial2 model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ shapeHelper 100 100 585 450 "blue"

            -- , Svg.rect
            --     [ SvgAttr.stroke "blue"
            --     , SvgAttr.strokeWidth "5"
            --     , SvgAttr.height "100"
            --     , SvgAttr.width "100"
            --     , SvgAttr.fillOpacity "0"
            --     , SvgAttr.x "950"
            --     , SvgAttr.y "450"
            --     ]
            --     []
            ]
        , dialogHelper 400 20 500 200 50 "blue" "Left click on Warrior to control it"

        -- , div
        -- [ HtmlAttr.style "width" "400px"
        -- , HtmlAttr.style "height" "20px"
        -- , HtmlAttr.style "position" "fixed"
        -- , HtmlAttr.style "left" "1150px"
        -- , HtmlAttr.style "top" "50px"
        -- , HtmlAttr.style "color" "blue"
        -- , HtmlAttr.style "font-size" "50px"
        -- ]
        -- [ text "Your heroes' Information (Blue)" ]
        ]


viewTutorial3 : Model -> Html Msg
viewTutorial3 model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ shapeHelper 100 100 420 450 "blue"
            ]
        , dialogHelper 400 20 500 250 50 "blue" "Left click on Warrior to control it"
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
