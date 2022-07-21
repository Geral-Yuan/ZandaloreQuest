module ViewNPCTask exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (..)
import Model exposing (Model)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr


viewSingleNPC : NPC -> List (Html Msg)
viewSingleNPC npc =
    let
        ( x, y ) =
            npc.position

        ( w, h ) =
            npc.size

        scaleFactor =
            case npc.faceDir of
                Left ->
                    -1

                _ ->
                    1
    in
    [ div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (toString (x - w / 2) ++ "px")
        , HtmlAttr.style "top" (toString (y - h / 2) ++ "px")
        ]
        [ img
            [ src ("./assets/image/" ++ npc.image ++ ".png")
            , height (floor w)
            , width (floor h)
            , HtmlAttr.style "transform" ("scaleX(" ++ toString scaleFactor ++ ")")
            ]
            []
        ]
    , viewChatBox ( x + 30 * scaleFactor, y - 30 ) scaleFactor
    ]


viewChatBox : ( Float, Float ) -> Int -> Html Msg
viewChatBox ( x, y ) scaleFactor =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (toString (x - 20) ++ "px")
        , HtmlAttr.style "top" (toString (y - 20) ++ "px")
        ]
        [ img
            [ src "./assets/image/ChatBox.gif"
            , height 40
            , width 40
            , HtmlAttr.style "transform" ("scaleX(" ++ toString scaleFactor ++ ")")
            ]
            []
        ]


viewTaskBoard : Svg msg
viewTaskBoard =
    Svg.rect
        [ SvgAttr.width "270"
        , SvgAttr.height "200"
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
        [ text ("Current Task: " ++ textTask model) ]


textTask : Model -> String
textTask model =
    case model.cntTask of
        MeetElder ->
            "Go to meet Elder and have the Tutorial Level!"

        GoToShop ->
            "Go to the shop and get the free Mage!"

        Level k ->
            "Go to talk with NPC " ++ toString k ++ " and beat him!"

        -- Maybe a name for each NPC later
        _ ->
            ""
