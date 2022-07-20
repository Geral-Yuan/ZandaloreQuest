module ViewNPCTask exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (..)
import Model exposing (Model)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr


viewDarkKnight : ( Float, Float ) -> Svg msg
viewDarkKnight ( x, y ) =
    Svg.image
        -- view dark knight
        [ SvgAttr.width "65"
        , SvgAttr.height "65"
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/EvilNPC.png"
        ]
        []


viewChatBox : ( Float, Float ) -> Html msg
viewChatBox ( x, y ) =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (toString x ++ "px")
        , HtmlAttr.style "top" (toString y ++ "px")
        ]
        [ img [ src "./assets/image/ChatBox.gif", height 40, width 40 ] []
        ]


viewShopKeeper : Svg msg
viewShopKeeper =
    Svg.image
        [ SvgAttr.width "85"
        , SvgAttr.height "85"
        , SvgAttr.x "750"
        , SvgAttr.y "380"
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/HealerRed.png"
        ]
        []


viewTaskBoard : Svg msg
viewTaskBoard =
    Svg.rect
        [ SvgAttr.width "270"
        , SvgAttr.height "150"
        , SvgAttr.x "1730"
        , SvgAttr.y "100"
        , SvgAttr.rx "20"
        , SvgAttr.fill "rgb(255,218,185)"
        ]
        []


viewTask : Model -> Html Msg
viewTask model =
    div
        [ HtmlAttr.style "top" "110px"
        , HtmlAttr.style "left" "1730px"
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "20px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (textTask model) ]


textTask : Model -> String
textTask model =
    case model.cntTask of
        MeetElder ->
            "Go to meet elder and have the Tutorial Level!"

        GoToShop ->
            "Go to the shop and get the free Mage!"

        Level k ->
            "Go to talk with NPC " ++ toString k ++ " and beat him!" -- Maybe a name for each NPC later

        _ ->
            ""
