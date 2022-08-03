module ViewDialog exposing (viewDialog)

{-| This file fills functions related to viewing the game dialog.


# Function

@docs viewDialog

-}

import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (Msg(..))
import NPC exposing (npcMap)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import Type exposing (Model, Task(..))
import ViewConst exposing (pixelHeight, pixelWidth)
import ViewOthers exposing (dialogHelper)
import ViewScenes exposing (viewCastleSvg, viewDungeonSvg)


{-| This function will display the dialog according to the task.
-}
viewDialog : Task -> Model -> Html Msg
viewDialog task model =
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
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewDialogBackground task
            , viewDialogBox
            ]
        , viewDialogMatch task
        ]


viewDialogMatch : Task -> Html Msg
viewDialogMatch task =
    case task of
        MeetElder ->
            viewDialogElder

        FinishTutorial ->
            viewFinishTutorial

        Level 1 ->
            viewDialogDarkKnight1

        Level 2 ->
            viewDialogDarkKnight2

        Level 3 ->
            viewDialogSkullKnight1

        Level 4 ->
            viewDialogSkullKnight2

        Level 5 ->
            viewDialogSkullKnight3

        Level 6 ->
            viewDialogBoss

        _ ->
            viewDialogGeneral


viewFinishTutorial : Html Msg
viewFinishTutorial =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "100px"
            , HtmlAttr.style "left" "350px"
            ]
            [ img [ src "./assets/image/MainCharacter.png", height 400, width 480 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "100px"
            , HtmlAttr.style "left" "1180px"
            , HtmlAttr.style "transform" "scaleX(-1)"
            ]
            [ img [ src "./assets/image/ElderNPC.png", height 400, width 480 ] []
            ]
        , dialogHelper 1300 450 370 560 50 "black" "Elder: Congratulations hero! The warrior and archer will be your comrades throughout this arduous journey. Now, head to the shop to recruit one more comrade. Click anywhere to continue."
        ]


viewDialogElder : Html Msg
viewDialogElder =
    let
        npcElder =
            npcMap 1
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ viewMainCharacterDialog
        , viewElderDialog
        , viewDialogContent npcElder.dialogue
        ]


viewDialogDarkKnight1 : Html Msg
viewDialogDarkKnight1 =
    let
        npcDarkKnight1 =
            npcMap 8
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ viewMainCharacterDialog
        , viewDarkKnightDialog
        , viewDialogContent npcDarkKnight1.dialogue
        ]


viewDialogDarkKnight2 : Html Msg
viewDialogDarkKnight2 =
    let
        npcDarkKnight2 =
            npcMap 9
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ viewMainCharacterDialog
        , viewDarkKnightDialog
        , viewDialogContent npcDarkKnight2.dialogue
        ]


viewDialogSkullKnight1 : Html Msg
viewDialogSkullKnight1 =
    let
        npcSkullKnight1 =
            npcMap 10
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ viewMainCharacterDialog
        , viewSkullKnightDialog
        , viewDialogContent npcSkullKnight1.dialogue
        ]


viewDialogSkullKnight2 : Html Msg
viewDialogSkullKnight2 =
    let
        npcSkullKnight2 =
            npcMap 11
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ viewMainCharacterDialog
        , viewSkullKnightDialog
        , viewDialogContent npcSkullKnight2.dialogue
        ]


viewDialogSkullKnight3 : Html Msg
viewDialogSkullKnight3 =
    let
        npcSkullKnight3 =
            npcMap 12
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ viewMainCharacterDialog
        , viewSkullKnightDialog
        , viewDialogContent npcSkullKnight3.dialogue
        ]


viewDialogBoss : Html Msg
viewDialogBoss =
    let
        npcBoss =
            npcMap 13
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ viewMainCharacterDialog
        , viewSkullKnightDialog
        , viewDialogContent npcBoss.dialogue
        ]


viewMainCharacterDialog : Html Msg
viewMainCharacterDialog =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "top" "100px"
        , HtmlAttr.style "left" "350px"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ img [ src "./assets/image/MainCharacter.png", height 400, width 480 ] []
        ]


viewDialogContent : String -> Html Msg
viewDialogContent string =
    div
        [ HtmlAttr.style "width" "1290px"
        , HtmlAttr.style "height" "450px"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "365px"
        , HtmlAttr.style "top" "570px"
        , HtmlAttr.style "color" "black"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "50px"
        ]
        [ text string ]


viewElderDialog : Html Msg
viewElderDialog =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "top" "100px"
        , HtmlAttr.style "left" "1180px"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "transform" "scaleX(-1)"
        ]
        [ img [ src "./assets/image/ElderNPC.png", height 400, width 480 ] []
        ]


viewDarkKnightDialog : Html Msg
viewDarkKnightDialog =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "top" "100px"
        , HtmlAttr.style "left" "1180px"
        , HtmlAttr.style "transform" "scaleX(-1)"
        ]
        [ img [ src "./assets/image/EvilNPC.png", height 400, width 480 ] []
        ]


viewSkullKnightDialog : Html Msg
viewSkullKnightDialog =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "top" "100px"
        , HtmlAttr.style "left" "1180px"
        , HtmlAttr.style "transform" "scaleX(-1)"
        ]
        [ img [ src "./assets/image/SkullKnight.png", height 400, width 480 ] []
        ]


viewDialogGeneral : Html Msg
viewDialogGeneral =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "50px"
            , HtmlAttr.style "left" "50px"
            ]
            [ img [ src "./assets/image/MainCharacter.png", height 300, width 380 ] []
            ]
        ]


viewDialogBox : Svg Msg
viewDialogBox =
    Svg.image
        [ SvgAttr.width "1600"
        , SvgAttr.height "700"
        , SvgAttr.x (toString (pixelWidth / 2 - 800))
        , SvgAttr.y (toString (pixelHeight / 2 - 100))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/dialogBox.png"
        ]
        []


viewDialogBackground : Task -> Svg Msg
viewDialogBackground task =
    case task of
        MeetElder ->
            viewCastleSvg

        Level 1 ->
            viewCastleSvg

        Level 2 ->
            viewCastleSvg

        _ ->
            viewDungeonSvg
