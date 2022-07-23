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

        3 ->
            viewTutorial3 model

        4 ->
            viewTutorial4 model

        5 ->
            viewTutorial5 model

        6 ->
            viewTutorial6 model

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
            [ shapeHelper 145 430 15 15 "blue" ( 0, 0 )
            , shapeHelper 450 440 1555 15 "blue" ( 0, 0 )
            , shapeHelper 100 100 950 450 "blue" ( 0, 0 )
            ]
        , dialogHelper 600 20 20 170 50 "blue" "Enemies Information (Red)"
        , dialogHelper 400 20 1150 50 50 "blue" "Your heroes' Information (Blue)"
        , dialogHelper 600 20 950 700 50 "blue" "Brown obstacle: mystery box that drops gold and potions (black means unbreakable)"
        , dialogHelper 600 20 20 900 50 "blue" "Click enter to continue"
        ]


viewTutorial2 : Model -> Html Msg
viewTutorial2 model =
    -- Select warrior
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
            [ shapeHelper 100 100 585 450 "blue" ( 0, 0 )

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
        , dialogHelper 500 20 500 200 50 "blue" "Left click on Warrior to control it"

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
            [ shapeHelper 100 100 705 450 "blue" ( 3, 7 )
            ]
        , dialogHelper 700 30 500 150 50 "blue" "Blue hexagons denote the movable range of the hero. Left click to move into the rectangle."
        ]


viewTutorial4 : Model -> Html Msg
viewTutorial4 model =
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
            [ shapeHelper 100 100 830 450 "blue" ( 4, 6 )
            ]
        , dialogHelper 800 30 580 150 50 "blue" "Each hero has an energy limit. -2 to move and -3 to attack. Energy refreshes after each turn. Left click to move."
        ]


viewTutorial5 : Model -> Html Msg
viewTutorial5 model =
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
            [ shapeHelper 100 100 645 350 "blue" ( 0, 0 )
            ]
        , dialogHelper 400 20 580 200 50 "blue" "Now left click on archer."
        ]


viewTutorial6 : Model -> Html Msg
viewTutorial6 model =
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
            [ shapeHelper 100 100 1010 345 "blue" ( 5, 4 )
            ]
        , dialogHelper 700 20 580 150 50 "blue" "Right click on the crate to attack. A random item (health/energy potion or gold) will be dropped."
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
