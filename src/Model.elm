module Model exposing (..)

import Board exposing (..)
import Browser.Dom exposing (getViewport)
import Data exposing (..)
import Html.Attributes exposing (class)
import Message exposing (..)


type alias Model =
    { heroes : List Hero
    , board : Board
    , size : ( Float, Float )
    , time : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )


initModel : Model
initModel =
    Model
        [ Hero Warrior ( 6, 6 ) 100 15 5 5 False
        , Hero Archer ( 5, 8 ) 40 20 3 5 False
        ]
        (initBoard 1)
        ( 1500, 1000 )
        0
