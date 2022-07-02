module Model exposing (..)

import Board exposing (Board, initBoard)
import Browser.Dom exposing (getViewport)
import Data exposing (..)
import Message exposing (Msg(..))
import Task


type alias Model =
    { mode : GameMode
    , heroes : List Hero
    , board : Board
    , size : ( Float, Float )

    -- , time : Float
    , clickPos : ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Task.perform GetViewport getViewport
    )


initModel : Model
initModel =
    Model
        BoardGame
        [ Hero Healer ( 6, 6 ) 100 5 5 5 False 1 -- heal 5 health
        , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
        , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
        ]
        (initBoard 1)
        ( 1500, 1000 )
        -- 0
        ( 0, 0 )
