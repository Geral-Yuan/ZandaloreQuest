module Main exposing (main)

{-| This file fills functions related to main.


# Function

@docs main

-}

import Browser exposing (element)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onKeyUp, onMouseMove, onResize)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Message exposing (Msg(..))
import Model exposing (init)
import Type exposing (Dir(..), Model)
import Update exposing (update)
import View exposing (view)


{-| This is the main function
-}
main : Program () Model Msg
main =
    element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp (Decode.map (key False) keyCode)
        , onKeyDown (Decode.map (key True) keyCode)
        , onResize Resize
        , onClick (Decode.map2 Click decodeFractionX decodeFractionY)
        , onMouseMove (Decode.map2 Point decodeFractionX decodeFractionY)
        ]


key : Bool -> Int -> Msg
key on keycode =
    case keycode of
        13 ->
            Enter on

        37 ->
            Key Left on

        38 ->
            Key Up on

        39 ->
            Key Right on

        40 ->
            Key Down on

        67 ->
            Talk on

        84 ->
            -- Key T
            Test

        _ ->
            Key_None


decodeFractionX : Decode.Decoder Float
decodeFractionX =
    Decode.map2 (/)
        (Decode.field "clientX" Decode.float)
        (Decode.at [ "currentTarget", "defaultView", "innerWidth" ] Decode.float)


decodeFractionY : Decode.Decoder Float
decodeFractionY =
    Decode.map2 (/)
        (Decode.field "clientY" Decode.float)
        (Decode.at [ "currentTarget", "defaultView", "innerWidth" ] Decode.float)
