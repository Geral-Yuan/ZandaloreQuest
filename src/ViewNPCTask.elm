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
            , width (floor w)
            , height (floor h)
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
            "Talk to a skull knight " ++ toString k ++ " and destroy him!"

        -- Maybe a name for each NPC later
        _ ->
            ""


checkTalkRange : Model -> Model
checkTalkRange model =
    let
        scene =
            case model.mode of
                Castle ->
                    CastleScene

                Shop ->
                    ShopScene

                Dungeon ->
                    DungeonScene

                _ ->
                    Dungeon2Scene
    in
    checkNPCTalk model (List.filter (\npc -> npc.scene == scene) model.npclist)


checkNPCTalk : Model -> List NPC -> Model
checkNPCTalk model npclist =
    let
        targetNPC =
            List.head (List.filter (checkInTalkRange model.character.pos) npclist)
    in
    case targetNPC of
        Nothing ->
            model

        Just npc ->
            if npc.beaten then
                model
                -- To be modified

            else
                { model | mode = Dialog model.cntTask, previousMode = Castle }


checkInTalkRange : ( Float, Float ) -> NPC -> Bool
checkInTalkRange ( x, y ) npc =
    let
        ( ( lx, rx ), ( uy, dy ) ) =
            npc.talkRange
    in
    x > lx && x < rx && y > uy && y < dy
