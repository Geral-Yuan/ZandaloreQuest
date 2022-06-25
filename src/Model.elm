module Model exposing (..)

import Data exposing (..)
import Browser.Dom exposing (getViewport)
import Html.Attributes exposing (class)
import Message exposing (..)
import Board exposing (..)

type alias Model =
    { characters : List Character
    , board : Board
    , size : (Float, Float)
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )


initModel : Model
initModel =
    Model [(Character Warrior (6, 6) 100 20 5)] (initBoard 1) (1000,1000)
