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
import ViewChoose exposing (viewHeroChoose, viewShopChoose)
import ViewOthers exposing (..)
import ViewScenes exposing (..)


viewTutorialScene : Int -> Model -> Html Msg
viewTutorialScene k model =
    case k of
        1 ->
            viewTutorial1 model

        _ ->
            viewTutorial2 model


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
            [ Svg.rect
                [ SvgAttr.stroke "blue"
                , SvgAttr.strokeWidth "5"
                , SvgAttr.height "145"
                , SvgAttr.width "430"
                , SvgAttr.fillOpacity "0"
                , SvgAttr.x "15"
                , SvgAttr.y "15"
                ]
                []
            , Svg.rect
                [ SvgAttr.stroke "blue"
                , SvgAttr.strokeWidth "5"
                , SvgAttr.height "450"
                , SvgAttr.width "440"
                , SvgAttr.fillOpacity "0"
                , SvgAttr.x "1555"
                , SvgAttr.y "15"
                ]
                []
            , Svg.rect
                [ SvgAttr.stroke "blue"
                , SvgAttr.strokeWidth "5"
                , SvgAttr.height "100"
                , SvgAttr.width "100"
                , SvgAttr.fillOpacity "0"
                , SvgAttr.x "950"
                , SvgAttr.y "450"
                ]
                []
            ]
        , div
            [ HtmlAttr.style "width" "600px"
            , HtmlAttr.style "height" "20px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "20px"
            , HtmlAttr.style "top" "170px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-size" "50px"
            ]
            [ text "Enemies Information (Red)" ]
        , div
            [ HtmlAttr.style "width" "400px"
            , HtmlAttr.style "height" "20px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "1150px"
            , HtmlAttr.style "top" "50px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-size" "50px"
            ]
            [ text "Your heroes' Information (Blue)" ]
        , div
            [ HtmlAttr.style "width" "600px"
            , HtmlAttr.style "height" "20px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "950px"
            , HtmlAttr.style "top" "700px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-size" "50px"
            ]
            [ text "Brown obstacle: mystery box that drops gold and potions (black means unbreakable)" ]
        , div
            [ HtmlAttr.style "width" "600px"
            , HtmlAttr.style "height" "20px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "20px"
            , HtmlAttr.style "top" "950px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-size" "50px"
            ]
            [ text "Click enter to continue" ]
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
            [ Svg.rect
                [ SvgAttr.stroke "blue"
                , SvgAttr.strokeWidth "5"
                , SvgAttr.height "145"
                , SvgAttr.width "440"
                , SvgAttr.fillOpacity "0"
                , SvgAttr.x "1555"
                , SvgAttr.y "15"
                ]
                []
            ]
        , div
            [ HtmlAttr.style "width" "400px"
            , HtmlAttr.style "height" "20px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "1150px"
            , HtmlAttr.style "top" "50px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-size" "50px"
            ]
            [ text "Click on Warrior to control it" ]
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
