module ViewShop exposing (..)

import ViewScenes exposing (..)
import Data exposing (pixelHeight, pixelWidth, Scene(..))
import Debug exposing (toString)
import Html.Attributes as HtmlAttr
import Message exposing (..)
import Model exposing (Model)
import Svg.Attributes as SvgAttr
import ViewNPCTask exposing (..)
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Svg exposing (Svg, text)
import ViewScenes exposing (viewBagCoin)

viewShop : Model -> Html Msg
viewShop model =
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
        (viewKeyGif
            ++ [ viewTask model
               , Svg.svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    ]
                    [ viewShopSvg
                    , viewTaskBoard
                    ]
               , viewCharacterPos model.character
               , viewTipForDir
               , viewTipForC
               , viewTipForEnter
               ]
            ++ List.concat (List.map viewSingleNPC (model.npclist |> List.filter (\x -> x.scene == ShopScene)))
            ++ [ viewRpgCharacter model.character ]
        )


viewShopSvg : Svg Msg
viewShopSvg =
    Svg.image
        [ SvgAttr.width "1600"
        , SvgAttr.height "1000"
        , SvgAttr.x (toString (pixelWidth / 2 - 800))
        , SvgAttr.y (toString (pixelHeight / 2 - 500))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Shop.jpg"
        ]
        []



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