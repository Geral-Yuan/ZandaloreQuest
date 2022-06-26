module Model exposing (..)

import Board exposing (..)
import Browser.Dom exposing (getViewport)
import Data exposing (..)
import Html.Attributes exposing (class)
import Message exposing (..)


type alias Model =
    { characters : List Character
    , board : Board
    , size : ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )


initModel : Model
initModel =
    Model
        [ Character Warrior ( 6, 6 ) 100 15 5 False
        , Character Archer ( 5, 8 ) 40 20 3 False
        ]
        (initBoard 1)
        ( 1500, 1000 )
