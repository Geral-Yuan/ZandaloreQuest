module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onKeyUp, onResize)
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

        72 ->
            -- Key H
            Hit on

        49 ->
            -- Key 1
            -- This might have to change because players can select 3 heroes from their inventory
            Select Warrior on

        50 ->
            -- Key 2
            Select Archer on

        51 ->
            -- Key 3
            Select Assassin on

        52 ->
            -- Key 4
            Select Mage on

        53 ->
            -- Key 5
            Select Healer on

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
