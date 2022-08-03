module ViewTutorial exposing (viewHintBackground, viewTutorialScene)

{-| This file fills functions related to viewing tutorial.


# Function

@docs viewHintBackground, viewTutorialScene

-}

import Data exposing (findFixedPos)
import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Message exposing (Msg(..))
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Type exposing (BoardState(..), Model, Turn(..))
import ViewOthers exposing (dialogHelper, shapeHelper)


{-| This function will display live tutorial step by step.
-}
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

            11 ->
                [ viewTutorial11 ]

            12 ->
                [ viewTutorial12 ]

            _ ->
                [ viewTutorial13 ]

    else
        []


{-| This function will display the tutorial text background.
-}
viewHintBackground : Float -> Float -> Float -> Float -> Svg msg
viewHintBackground w h x y =
    Svg.rect
        [ SvgAttr.width (toString w)
        , SvgAttr.height (toString h)
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.fill "black"
        , SvgAttr.fillOpacity "70%"
        ]
        []


viewTutorial1 : Html Msg
viewTutorial1 =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ shapeHelper ( 150, 430 ) ( 230, 90 ) "blue" ( 0, 0 )
            , shapeHelper ( 300, 430 ) ( 1780, 165 ) "blue" ( 0, 0 )
            , shapeHelper ( 100, 100 ) (findFixedPos ( 5, 5 )) "blue" ( 0, 0 )
            , viewHintBackground 600 70 20 170
            , viewHintBackground 290 170 1240 50
            , viewHintBackground 600 240 940 700
            , viewHintBackground 800 70 20 900
            ]
        , dialogHelper 600 20 20 170 50 "white" "Enemies Information (Red)"
        , dialogHelper 400 20 1250 50 50 "white" "Your heroes' Information (Blue)"
        , dialogHelper 600 20 950 700 50 "white" "Brown obstacle: mystery box that drops gold and potions (black means unbreakable)"
        , dialogHelper 800 20 20 900 50 "white" "Click anywhere to continue"
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
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 2, 8 )) "blue" ( 0, 0 )
            , viewHintBackground 500 130 500 50
            ]
        , dialogHelper 500 20 500 50 50 "white" "Left click on Warrior to control it"
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 3, 7 )) "blue" ( 3, 7 )
            , viewHintBackground 700 250 500 50
            ]
        , dialogHelper 700 30 500 50 50 "white" "Blue hexagons denote the attackable range of the hero. Left click the adjacent hexagon to move."
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 4, 6 )) "blue" ( 4, 6 )
            , viewHintBackground 800 200 580 50
            ]
        , dialogHelper 800 30 580 50 50 "white" "Each hero has an energy limit. -2 to move and -3 to attack. Energy refreshes after each turn. Left click to move."
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 2, 7 )) "blue" ( 0, 0 )
            , viewHintBackground 400 130 580 50
            ]
        , dialogHelper 400 20 580 50 50 "white" "Now left click on archer."
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 5, 4 )) "blue" ( 5, 4 )
            , viewHintBackground 700 210 580 50
            ]
        , dialogHelper 700 20 580 50 50 "white" "Right click on the crate to attack. A random item (health/energy potion or gold) will be dropped."
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
            [ Svg.rect
                [ SvgAttr.stroke "blue"
                , SvgAttr.strokeWidth "5"
                , SvgAttr.height "110"
                , SvgAttr.width "210"
                , SvgAttr.fill "blue"
                , SvgAttr.x "1680"
                , SvgAttr.y "880"
                , SvgAttr.fill "transparent"
                ]
                []
            , viewHintBackground 700 190 850 800
            ]
        , dialogHelper 700 20 850 800 50 "white" "Click the end turn button to end your turn wait till the enemy turn is over."
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 4, 6 )) "blue" ( 4, 6 )
            , viewHintBackground 700 130 580 50
            ]
        , dialogHelper 700 20 580 50 50 "white" "Now it is your turn, left click on the warrior"
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 4, 5 )) "blue" ( 4, 5 )
            , viewHintBackground 700 70 580 50
            ]
        , dialogHelper 700 20 580 50 50 "white" "Now move it here"
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 5, 4 )) "blue" ( 5, 4 )
            , viewHintBackground 700 190 580 50
            ]
        , dialogHelper 700 20 580 50 50 "white" "Move the warrior onto the item and its energy will be full."
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 2, 7 )) "blue" ( 2, 7 )
            , viewHintBackground 400 130 580 50
            ]
        , dialogHelper 400 20 580 50 50 "white" "Now left click on archer."
        ]


viewTutorial12 : Html Msg
viewTutorial12 =
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
            [ shapeHelper ( 100, 100 ) (findFixedPos ( 7, 2 )) "blue" ( 7, 2 )
            , viewHintBackground 600 210 580 50
            ]
        , dialogHelper 600 20 580 50 50 "white" "Now right click on the enemy hero to attack it. Crticial damage may be dealt."
        ]


viewTutorial13 : Html Msg
viewTutorial13 =
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
            [ viewHintBackground 850 250 580 50 ]
        , dialogHelper 850 30 580 50 50 "white" "I have been guiding you and now it is time for you to destroy the enemy by yourself. Good luck hero! Click anywhere to continue."
        ]
