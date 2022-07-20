module ViewNPCTask exposing (..)

import Message exposing (Msg)
import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (..)
import Model exposing (Model)
import RpgCharacter exposing (RpgCharacter)
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

viewChatBox : (Float, Float) -> Svg msg
viewChatBox (x,y) =
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
