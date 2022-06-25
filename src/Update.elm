module Update exposing (update)

import Data exposing (..)
import Model exposing (Model)
import Message exposing (Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (model, Cmd.none)