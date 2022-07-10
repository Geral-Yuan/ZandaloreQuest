module ViewChoose exposing (viewHeroChoose, viewShopChoose)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (Model)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import ViewOthers exposing (viewBoardCoin)
import ViewScenes exposing (viewBagCoin)


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
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "grey"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (List.map viewYourHeroes (List.filter (\( _, y ) -> y /= 0) model.indexedheroes)
                ++ List.map viewFrame model.chosenHero
            )
        , confirmButton
        ]


viewShopChoose : Model -> Html Msg
viewShopChoose model =
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
        , HtmlAttr.style "background" "black"
        ]
        [ healthButton
        , damageButton
        , exitButton
        , viewBagCoin model
        ]


healthButton : Html Msg
healthButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "400px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "200px"
        , HtmlAttr.style "left" "1000px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "400px"
        , onClick UpgradeHealth
        ]
        [ text "50 coins to upgrade (+5) health of all heroes" ]


exitButton : Html Msg
exitButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "920px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "50px"
        , HtmlAttr.style "left" "1500px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "100px"
        , onClick ExitShop
        ]
        [ text "Exit" ]


damageButton : Html Msg
damageButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "400px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "200px"
        , HtmlAttr.style "left" "500px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "400px"
        , onClick UpgradeDamage
        ]
        [ text "50 coins to upgrade (+2) damage of all heroes" ]


viewFrame : Int -> Svg Msg
viewFrame index =
    let
        y =
            ((index - 1) // 3) * 500 + 50

        x =
            modBy 3 (index - 1) * 600 + 200
    in
    Svg.rect
        [ SvgAttr.width "400"
        , SvgAttr.height "400"
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
            ((index - 1) // 3) * 500 + 100

        x =
            modBy 3 (index - 1) * 600 + 250

        class =
            toString hero.class
    in
    Svg.image
        [ SvgAttr.width "300"
        , SvgAttr.height "300"
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Blue.png")
        ]
        []


confirmButton : Html Msg
confirmButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "910px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "left" "880px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "300px"
        , onClick Confirm
        ]
        [ text "Choose 3 heroes and Confirm" ]
