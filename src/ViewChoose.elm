module ViewChoose exposing (viewHeroChoose)

import Data exposing (pixelHeight, pixelWidth)
import Debug exposing (toString)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import Type exposing (Hero, Model)
import ViewOthers exposing (viewUIButton, viewUIFrame)


viewHeroChoose : Model -> Html Msg
viewHeroChoose model =
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
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "grey"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (List.map viewYourHeroes (List.filter (\( _, y ) -> y /= 0) model.indexedheroes)
                ++ List.map viewFrame model.chosenHero
                ++ viewUIFrame 600 125 700 25
                ++ viewUIButton 250 100 875 875
            )
        , viewChooseText
        , confirmButton
        ]


viewFrame : Int -> Svg Msg
viewFrame index =
    let
        y =
            ((index - 1) // 3) * 325 + 200

        x =
            modBy 3 (index - 1) * 450 + 400
    in
    Svg.rect
        [ SvgAttr.width "300"
        , SvgAttr.height "300"
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.rx "20"
        , SvgAttr.fill "transparent"
        , SvgAttr.stroke "gold"
        , SvgAttr.strokeWidth "10"
        ]
        []


viewYourHeroes : ( Hero, Int ) -> Svg Msg
viewYourHeroes ( hero, index ) =
    let
        y =
            ((index - 1) // 3) * 325 + 225

        x =
            modBy 3 (index - 1) * 450 + 425

        class =
            toString hero.class
    in
    Svg.image
        [ SvgAttr.width "250"
        , SvgAttr.height "250"
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Blue.png")
        ]
        []


viewChooseText : Svg msg
viewChooseText =
    div
        [ HtmlAttr.style "top" "50px"
        , HtmlAttr.style "left" "700px"
        , HtmlAttr.style "width" "600px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Choose 3 Heroes" ]


confirmButton : Html Msg
confirmButton =
    button
        [ HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "top" "875px"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "25px"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "height" "100px"
        , HtmlAttr.style "left" "875px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "250px"
        , HtmlAttr.style "border" "transparent"
        , HtmlAttr.style "text-align" "center"
        , onClick Confirm
        ]
        [ text "Confirm" ]
