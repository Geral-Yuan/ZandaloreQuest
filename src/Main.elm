module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onKeyUp, onMouseMove, onResize)
import Data exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Message exposing (Msg(..))
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
        , onClick (Decode.map2 Click decodeFractionX decodeFractionY)
        , onMouseMove (Decode.map2 Point decodeFractionX decodeFractionY)
        ]


key : Bool -> Int -> Msg
key on keycode =
    case keycode of
        --        87 ->
        --            Key W on
        --
        --        69 ->
        --            Key E on
        --
        --        68 ->
        --            Key D on
        --
        --        88 ->
        --            Key X on
        --
        --        90 ->
        --            Key Z on
        --
        --        65 ->
        --            Key A on
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

        75 ->
            -- Key K
            Kill on

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
