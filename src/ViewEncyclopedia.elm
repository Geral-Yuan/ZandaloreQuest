module ViewEncyclopedia exposing (viewEncyclopedia, viewEncyclopediaButton)

import Data exposing (Class(..), pixelHeight, pixelWidth)
import Debug exposing (toString)
import Html exposing (Html, button, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import ViewOthers exposing (dialogHelper, viewUIFrame)


viewEncyclopedia : Class -> Model -> Html Msg
viewEncyclopedia class model =
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
        , HtmlAttr.style "background" "rgb(184,111,80)"
        ]
        ([ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            ([]
                ++ viewUIFrame 1200 800 400 200
                ++ viewHeroInformation
            )
         , rightEncyclopediaButton
         , leftEncyclopediaButton
         , exitEncyclopediaButton
         , encyclopediaHero class
         ]
            ++ viewDescription class
            ++ viewHeroInfo class
        )


viewHeroInfo : Class -> List (Html Msg)
viewHeroInfo class =
    let
        health =
            case class of
                Warrior ->
                    80

                Archer ->
                    30

                Assassin ->
                    35

                Mage ->
                    50

                Healer ->
                    40

                Engineer ->
                    30

                _ ->
                    50

        damage =
            case class of
                Warrior ->
                    15

                Archer ->
                    20

                Assassin ->
                    20

                Mage ->
                    12

                Healer ->
                    5

                Engineer ->
                    5

                _ ->
                    50

        energy =
            case class of
                Assassin ->
                    6

                Mage ->
                    3

                _ ->
                    5
    in
    [ div
        [ HtmlAttr.style "top" "300px"
        , HtmlAttr.style "left" "1350px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "50px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString health) ]
    , div
        [ HtmlAttr.style "top" "400px"
        , HtmlAttr.style "left" "1350px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "50px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString damage) ]
    , div
        [ HtmlAttr.style "top" "500px"
        , HtmlAttr.style "left" "1350px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "50px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString energy) ]
    ]


viewDescription : Class -> List (Html Msg)
viewDescription class =
    case class of
        Warrior ->
            [ dialogHelper 1100 20 500 700 40 "white" "- Attack range: any one hexagon around the warrior"
            , dialogHelper 1100 20 500 770 40 "white" "- Specialty: high health and tanky"
            , dialogHelper 1100 20 800 50 120 "white" "Warrior"
            ]

        Archer ->
            [ dialogHelper 1100 20 500 700 40 "white" "- Attack range: any one hexagon within the 6 lanes around archer"
            , dialogHelper 1100 20 500 770 40 "white" "- Specialty: high damage but health is low"
            , dialogHelper 1100 20 800 50 120 "white" "Archer"
            ]

        Mage ->
            [ dialogHelper 1100 20 500 700 40 "white" "- Attack range: any one hexagon around the mage which is 2 steps away"
            , dialogHelper 1100 20 500 770 40 "white" "- Specialty: heros around the hexagon that was clicked will be damaged too"
            , dialogHelper 1100 20 800 50 120 "white" "Mage"
            ]

        Assassin ->
            [ dialogHelper 1100 20 500 700 40 "white" "- Attack range: any one hexagon around the warrior"
            , dialogHelper 1100 20 500 770 40 "white" "- Specialty: fast (more energy to move/attack)"
            , dialogHelper 1100 20 800 50 120 "white" "Assassin"
            ]

        Healer ->
            [ dialogHelper 1100 20 500 700 40 "white" "- Attack range: any one hexagon around the healer"
            , dialogHelper 1100 20 500 770 40 "white" "- Specialty: heal teammates"
            , dialogHelper 1100 20 800 50 120 "white" "Healer"
            ]

        _ ->
            [ dialogHelper 1100 20 500 700 40 "white" "- Attack range: any one hexagon around the warrior"
            , dialogHelper 1100 20 500 770 40 "white" "- Specialty: create turrets that can damage enemies"
            , dialogHelper 1100 20 800 50 120 "white" "Engineer"
            ]


viewHeroInformation : List (Svg Msg)
viewHeroInformation =
    [ Svg.image
        [ SvgAttr.width "80"
        , SvgAttr.height "80"
        , SvgAttr.x "1150"
        , SvgAttr.y "300"
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Heart.png"
        ]
        []
    , Svg.image
        [ SvgAttr.width "80"
        , SvgAttr.height "80"
        , SvgAttr.x "1150"
        , SvgAttr.y "400"
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Sword.png"
        ]
        []
    , Svg.image
        [ SvgAttr.width "80"
        , SvgAttr.height "80"
        , SvgAttr.x "1150"
        , SvgAttr.y "500"
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Energy.png"
        ]
        []
    ]


encyclopediaHero : Class -> Html Msg
encyclopediaHero class =
    let
        fimage =
            "./assets/image/" ++ toString class
    in
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "top" "250px"
        , HtmlAttr.style "left" "500px"
        ]
        [ img [ src (fimage ++ "BlueGIF.gif"), height 400, width 575 ] []
        ]


rightEncyclopediaButton : Html Msg
rightEncyclopediaButton =
    button
        [ HtmlAttr.style "background" "url('./assets/image/rightArrow.png')"
        , HtmlAttr.style "top" "520px"

        -- , HtmlAttr.style "color" "white"
        , HtmlAttr.style "height" "160px"
        , HtmlAttr.style "left" "1700px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "100px"
        , HtmlAttr.style "border" "transparent"
        , onClick RightEncyclopedia
        ]
        []


leftEncyclopediaButton : Html Msg
leftEncyclopediaButton =
    button
        [ HtmlAttr.style "background" "url('./assets/image/rightArrow.png')"
        , HtmlAttr.style "top" "520px"
        , HtmlAttr.style "transform" "scaleX(-1)"

        -- , HtmlAttr.style "color" "white"
        , HtmlAttr.style "height" "160px"
        , HtmlAttr.style "left" "200px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "100px"
        , HtmlAttr.style "border" "transparent"
        , onClick LeftEncyclopedia
        ]
        []


exitEncyclopediaButton : Html Msg
exitEncyclopediaButton =
    button
        [ HtmlAttr.style "background" "url('./assets/image/cancel.png') no-repeat fixed"
        , HtmlAttr.style "object-fit" "cover"
        , HtmlAttr.style "top" "50px"

        -- , HtmlAttr.style "color" "white"
        , HtmlAttr.style "height" "100px"
        , HtmlAttr.style "left" "50px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "100px"
        , HtmlAttr.style "border" "transparent"
        , onClick Back
        ]
        []


viewEncyclopediaButton : Html Msg
viewEncyclopediaButton =
    button
        [ HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "top" "800px"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "left" "50px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "170px"
        , HtmlAttr.style "border" "transparent"
        , onClick SeeEncyclopedia
        ]
        [ text "Encyclopedia" ]