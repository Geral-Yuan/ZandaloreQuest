module ViewScenes exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, button, div, img, input, text)
import Html.Attributes as HtmlAttr exposing (height, src, style, width)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (Model)
import Svg exposing (Svg, stop)
import Svg.Attributes as SvgAttr


logoWidth : Float
logoWidth =
    523.2558


logoHeight : Float
logoHeight =
    600


determineOpct : Float -> Float
determineOpct t =
    if t <= 6 then
        sin (t * pi / 6) * 2 / 1.732

    else
        0


viewScene0 : Model -> Html Msg
viewScene0 model =
    let
        ( w, h ) =
            model.size

        -- t =
        --     model.time
        r =
            if w / h > logoWidth / logoHeight then
                Basics.min 1 (h / logoHeight)

            else
                Basics.min 1 (w / logoWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat (logoWidth + 400) ++ "px")
        , HtmlAttr.style "height" (String.fromFloat (logoHeight + 400) ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat (550 + (w - logoWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - logoHeight * r) / 2) ++ "px")

        -- , HtmlAttr.style "opacity" (determineOpct t |> String.fromFloat)
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , ("url('./assets/image/logo.png')" ++ " no-repeat fixed " ++ " 0px " ++ " 0px / " ++ " 523.2558px " ++ " 600px")
            |> HtmlAttr.style "background"
        ]
        []



-- Created this code so that the RPG character can change facing but there are bugs
-- viewRpgCharacter : Model -> Html Msg
-- viewRpgCharacter model =
--     div
--         [ style "position" "absolute"
--         , style "top" (toString (Tuple.first model.character.pos))
--         , style "left" (toString (Tuple.first model.character.pos))
--         , style "z-index" "9999"
--         ]
--         [ case model.character.faceDir of
--             Left ->
--                 img
--                     [ src "./assets/image/WarriorGood.png"
--                     , height (floor model.character.height)
--                     , width (floor model.character.width)
--                     , style "transform" "scaleX(-1)"
--                     ]
--                     []
--             Right ->
--                 img
--                     [ src "./assets/image/WarriorGood.png"
--                     , height (floor model.character.height)
--                     , width (floor model.character.width)
--                     ]
--                     []
--             _ ->
--                 img
--                     [ src "./assets/image/WarriorGood.png"
--                     , height (floor model.character.height)
--                     , width (floor model.character.width)
--                     ]
--                     []
--         ]


viewRpgCharacter : Model -> Svg Msg
viewRpgCharacter model =
    Svg.image
        [ SvgAttr.width (toString model.character.width)
        , SvgAttr.height (toString model.character.height)
        , SvgAttr.x (toString (Tuple.first model.character.pos))
        , SvgAttr.y (toString (Tuple.second model.character.pos))
        , SvgAttr.preserveAspectRatio "xMidYMid slice"
        , SvgAttr.xlinkHref "./assets/image/WarriorGood.png"
        ]
        []


viewCastle : Model -> Html Msg
viewCastle model =
    -- Add this when the homepage has been designed
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
        , HtmlAttr.style "left" (String.fromFloat (560 + (w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , ("url('./assets/image/Castle.png')"
            ++ " no-repeat fixed "
            ++ " 0px "
            ++ " 0px / "
            ++ (toString startWidth ++ "px " ++ (toString startHeight ++ "px"))
          )
            |> HtmlAttr.style "background"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewRpgCharacter model ]
        ]


viewShop : Model -> Html Msg
viewShop model =
    -- Add this when the homepage has been designed
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
        , HtmlAttr.style "left" (String.fromFloat (560 + (w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , ("url('./assets/image/Shop.jpg')"
            ++ " no-repeat fixed "
            ++ " 0px "
            ++ " 0px / "
            ++ (toString startWidth ++ "px " ++ (toString startHeight ++ "px"))
          )
            |> HtmlAttr.style "background"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewRpgCharacter model ]
        ]


startWidth : Float
startWidth =
    2000


startHeight : Float
startHeight =
    1000


viewStarting : Model -> Html Msg
viewStarting model =
    -- Add this when the homepage has been designed
    let
        ( w, h ) =
            model.size

        r =
            if w / h > startWidth / startHeight then
                Basics.min 1 (h / startHeight)

            else
                Basics.min 1 (w / startWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat startWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat startHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat (600 + (w - startWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - startHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , ("url('./assets/image/Start.png')"
            ++ " no-repeat fixed "
            ++ " 0px "
            ++ " 0px / "
            ++ (toString startWidth ++ "px " ++ (toString startHeight ++ "px"))
          )
            |> HtmlAttr.style "background"
        ]
        []
