module ViewNPCTask exposing (checkTalkRange, viewSingleNPC, viewTask, viewTaskBoard)

{-| This file fills functions related to viewing NPC tasks.


# Function

@docs checkTalkRange, viewSingleNPC, viewTask, viewTaskBoard

-}

import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (Msg(..))
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import Type exposing (Dir(..), FailToDo(..), GameMode(..), Model, NPC, Scene(..), Task(..))


{-| This function will display an NPC's image according to its name.
-}
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
        , HtmlAttr.style "font-family" "myfont"
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


{-| This function will display the task board outline and properties.
-}
viewTaskBoard : List (Svg msg)
viewTaskBoard =
    [ Svg.rect
        -- outer frame
        [ SvgAttr.width "270"
        , SvgAttr.height "200"
        , SvgAttr.x "1730"
        , SvgAttr.y "30"
        , SvgAttr.fill "rgb(184,111,80)"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        ]
        []
    , Svg.rect
        -- inner frame
        [ SvgAttr.width "250"
        , SvgAttr.height "180"
        , SvgAttr.x "1740"
        , SvgAttr.y "40"
        , SvgAttr.fill "rgb(63,40,50)"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        ]
        []
    ]


{-| This function will display task for player to follow.
-}
viewTask : Model -> Html Msg
viewTask model =
    div
        [ HtmlAttr.style "top" "40px"
        , HtmlAttr.style "left" "1745px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "22px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "left"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("Task: " ++ textTask model) ]


textTask : Model -> String
textTask model =
    case model.cntTask of
        MeetElder ->
            "Meet the Elder to complete the Tutorial Level!"

        GoToShop ->
            "Go to the shop and get a free random hero!"

        Level 1 ->
            "Talk to a Dark Knight 1 and defeat him!"

        Level 2 ->
            "Talk to Dark Knight 2 and defeat him!"

        Level 3 ->
            "Enter Dungeon and destroy the Skull Knight!"

        Level 4 ->
            "Skull Knight 2 appears! Kill him!"

        Level 5 ->
            "Enter the Side Dungeon and fight Skull Knight 3"

        -- Maybe a name for each NPC later
        _ ->
            ""


{-| This function will allow player to talk to the NPCs.
-}
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
            if model.test then
                { model | level = npc.level, mode = Dialog npc.task, previousMode = model.mode }

            else if npc.beaten then
                if Tuple.first model.popUpHint == Noop then
                    { model | popUpHint = ( FailtoTalk npc, 0 ) }

                else
                    model

            else
                { model | mode = Dialog model.cntTask, previousMode = model.mode }


checkInTalkRange : ( Float, Float ) -> NPC -> Bool
checkInTalkRange ( x, y ) npc =
    let
        ( ( lx, rx ), ( uy, dy ) ) =
            npc.talkRange
    in
    x > lx && x < rx && y > uy && y < dy
