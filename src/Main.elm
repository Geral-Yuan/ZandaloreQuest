module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Data exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Message exposing (..)
import Model exposing (Model, init)
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp (Decode.map (key False) keyCode)
        , onKeyDown (Decode.map (key True) keyCode)
        , onResize Resize
        ]


key : Bool -> Int -> Msg
key on keycode =
    case keycode of
        87 ->
            Key W on

        69 ->
            Key E on

        68 ->
            Key D on

        88 ->
            Key X on

        90 ->
            Key Z on

        65 ->
            Key A on

        _ ->
            Key_None
