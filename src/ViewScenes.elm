module ViewScenes exposing (..)

import Debug exposing (toString)
import Html exposing (Html, button, div, text)
import Html.Attributes as HtmlAttr exposing (style)
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
        [ HtmlAttr.style "width" (String.fromFloat logoWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat logoHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - logoWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - logoHeight * r) / 2) ++ "px")

        -- , HtmlAttr.style "opacity" (determineOpct t |> String.fromFloat)
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , ("url('./assets/image/logo.png')" ++ " no-repeat fixed " ++ " 0px " ++ " 0px / " ++ " 523.2558px " ++ " 600px")
            |> HtmlAttr.style "background"
        ]
        []


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


viewRoom1 : Model -> Html Msg
viewRoom1 model =
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
        , HtmlAttr.style "left" (String.fromFloat ((w - startWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - startHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , ("url('./assets/image/background.png')"
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
    1200


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
        , HtmlAttr.style "left" (String.fromFloat ((w - startWidth * r) / 2) ++ "px")
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
