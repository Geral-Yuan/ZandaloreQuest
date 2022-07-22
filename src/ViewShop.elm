module ViewShop exposing (..)

import ViewScenes exposing (..)
import Data exposing (pixelHeight, pixelWidth, Scene(..))
import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Message exposing (..)
import Model exposing (Model)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import ViewNPCTask exposing (..)


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