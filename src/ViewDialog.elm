module ViewDialog exposing (viewDialog)

import Data exposing (Task(..), pixelHeight, pixelWidth)
import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import ViewOthers exposing (dialogHelper)
import ViewScenes exposing (viewDungeonSvg)


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
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewDungeonSvg
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
        , dialogHelper 1300 450 370 560 50 "blue" "Elder: Congratulations hero! The warrior and archer will be your comrades throughout this arduous journey. Now, head to the shop to recruit one more comrade. Click Enter to continue."
        ]


viewDialogElder : Html Msg
viewDialogElder =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
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
        , div
            [ HtmlAttr.style "width" "1300px"
            , HtmlAttr.style "height" "450px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "370px"
            , HtmlAttr.style "top" "560px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-size" "50px"
            ]
            [ text "Elder: Welcome to the tutorial! The warrior and archer will help you on this arduous journey, choose one more hero to join you in this tutorial. Click Enter to countinue." ]
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
        [ SvgAttr.width "1500"
        , SvgAttr.height "500"
        , SvgAttr.x (toString (pixelWidth / 2 - 750))
        , SvgAttr.y (toString (pixelHeight / 2))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/DialogBox.png"
        ]
        []
