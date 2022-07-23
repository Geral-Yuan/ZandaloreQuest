module ViewTutorial exposing (..)

import Data exposing (..)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import ViewAllEnemy exposing (..)
import ViewAllHero exposing (..)
import ViewOthers exposing (..)
import ViewScenes exposing (..)


viewTutorialScene : Model -> Int -> List (Html Msg)
viewTutorialScene model k =
    if model.board.boardState == NoActions && model.board.turn == PlayerTurn then
        case k of
            1 ->
                [ viewTutorial1 ]

            2 ->
                [ viewTutorial2 ]

            3 ->
                [ viewTutorial3 ]

            4 ->
                [ viewTutorial4 ]

            5 ->
                [ viewTutorial5 ]

            6 ->
                [ viewTutorial6 ]

            7 ->
                [ viewTutorial7 ]

            8 ->
                [ viewTutorial8 ]

            9 ->
                [ viewTutorial9 ]

            10 ->
                [ viewTutorial10 ]

            _ ->
                [ viewTutorial11 ]

    else
        []


viewTutorial1 : Html Msg
viewTutorial1 =
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
            [ shapeHelper ( 145, 430 ) ( 230, 87.5 ) "blue" ( 0, 0 )
            , shapeHelper ( 450, 430 ) ( 1780, 240 ) "blue" ( 0, 0 )
            , shapeHelper ( 100, 100 ) (findPos ( 5, 5 )) "blue" ( 0, 0 )
            ]
        , dialogHelper 600 20 20 170 50 "blue" "Enemies Information (Red)"
        , dialogHelper 400 20 1150 50 50 "blue" "Your heroes' Information (Blue)"
        , dialogHelper 600 20 950 700 50 "blue" "Brown obstacle: mystery box that drops gold and potions (black means unbreakable)"
        , dialogHelper 600 20 20 900 50 "blue" "Click enter to continue"
        ]


viewTutorial2 : Html Msg
viewTutorial2 =
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
            [ shapeHelper ( 100, 100 ) (findPos ( 2, 8 )) "blue" ( 0, 0 )

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
        , dialogHelper 500 20 500 50 50 "blue" "Left click on Warrior to control it"

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


viewTutorial3 : Html Msg
viewTutorial3 =
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
            [ shapeHelper ( 100, 100 ) (findPos ( 3, 7 )) "blue" ( 3, 7 )
            ]
        , dialogHelper 700 30 500 50 50 "blue" "Blue hexagons denote the movable range of the hero. Left click to move into the rectangle."
        ]


viewTutorial4 : Html Msg
viewTutorial4 =
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
            [ shapeHelper ( 100, 100 ) (findPos ( 4, 6 )) "blue" ( 4, 6 )
            ]
        , dialogHelper 800 30 580 50 50 "blue" "Each hero has an energy limit. -2 to move and -3 to attack. Energy refreshes after each turn. Left click to move."
        ]


viewTutorial5 : Html Msg
viewTutorial5 =
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
            [ shapeHelper ( 100, 100 ) (findPos ( 2, 7 )) "blue" ( 0, 0 )
            ]
        , dialogHelper 400 20 580 50 50 "blue" "Now left click on archer."
        ]


viewTutorial6 : Html Msg
viewTutorial6 =
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
            [ shapeHelper ( 100, 100 ) (findPos ( 5, 4 )) "blue" ( 5, 4 )
            ]
        , dialogHelper 700 20 580 50 50 "blue" "Right click on the crate to attack. A random item (health/energy potion or gold) will be dropped."
        ]


viewTutorial7 : Html Msg
viewTutorial7 =
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
            []
        , dialogHelper 700 20 580 50 50 "blue" "There is a chance that critical damage will be dealt. Click the end turn button to end your turn."
        ]


viewTutorial8 : Html Msg
viewTutorial8 =
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
            [ shapeHelper ( 100, 100 ) (findPos ( 4, 6 )) "blue" ( 4, 6 )
            ]
        , dialogHelper 700 20 580 50 50 "blue" "Now it is your turn, click on the warrior"
        ]


viewTutorial9 : Html Msg
viewTutorial9 =
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
            [ shapeHelper ( 100, 100 ) (findPos ( 4, 5 )) "blue" ( 4, 5 )
            ]
        , dialogHelper 700 20 580 50 50 "blue" "Now move it here"
        ]


viewTutorial10 : Html Msg
viewTutorial10 =
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
            [ shapeHelper ( 100, 100 ) (findPos ( 5, 4 )) "blue" ( 5, 4 )
            ]
        , dialogHelper 700 20 580 50 50 "blue" "Move the warrior onto the item and it will receive +10 health/energy."
        ]


viewTutorial11 : Html Msg
viewTutorial11 =
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
            []
        , dialogHelper 700 30 580 50 50 "blue" "I have been guiding you and now it is time for you to destroy the enemy by yourself. Good luck hero!"
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
