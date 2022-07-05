module Model exposing (..)

import Board exposing (Board, initBoard)
import Browser.Dom exposing (getViewport)
import Data exposing (..)
import Json.Decode exposing (int)
import Message exposing (Msg(..))
import RpgCharacter exposing (..)
import Svg.Attributes exposing (mode)
import Task


type alias Model =
    { mode : GameMode
    , heroes : List Hero
    , board : Board
    , size : ( Float, Float )
    , character : RpgCharacter

    -- , time : Float
    , clickPos : ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Task.perform GetViewport getViewport
    )


initCharacter : RpgCharacter
initCharacter =
    { pos = ( 500, 1000 )
    , moveLeft = False
    , moveRight = False
    , moveUp = False
    , moveDown = False
    , latestDir = Left
    , height = 80
    , width = 80
    , speed = 500
    , move_range = ( pixelWidth, pixelHeight )
    }


initModel : Model
initModel =
    { mode = Scene 0
    , heroes =
        [ Hero Healer ( 6, 6 ) 100 5 5 5 False 1 -- heal 5 health
        , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
        , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
        ]
    , board = initBoard 1
    , size = ( 1500, 1000 )
    , character = initCharacter

    -- , time = 0
    , clickPos = ( 0, 0 )
    }


initRoom : Int -> Model -> Model
initRoom k model =
    case k of
        _ ->
            room_1 model


initLevel : Int -> Model -> Model
initLevel k model =
    case k of
        _ ->
            board_1 model


room_1 : Model -> Model
room_1 model =
    { mode = Room 1
    , heroes =
        [ Hero Healer ( 6, 6 ) 100 5 5 5 False 1 -- heal 5 health
        , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
        , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
        ]
    , board = initBoard 1
    , size = ( 1500, 1000 )
    , character = initCharacter

    -- , time = 0
    , clickPos = ( 0, 0 )
    }


board_1 : Model -> Model
board_1 model =
    { mode = BoardGame 1
    , heroes =
        [ Hero Healer ( 6, 6 ) 100 5 5 5 False 1 -- heal 5 health
        , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
        , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
        ]
    , board = initBoard 1
    , size = ( 1500, 1000 )
    , character = initCharacter

    -- , time = 0
    , clickPos = ( 0, 0 )
    }
