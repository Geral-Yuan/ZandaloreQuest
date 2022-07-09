module Model exposing (..)

import Board exposing (Board, initBoard)
import Browser.Dom exposing (getViewport)
import Data exposing (..)
import Message exposing (Msg(..))
import RpgCharacter exposing (..)
import Task


type alias Model =
    { mode : GameMode
    , heroes : List Hero
    , board : Board
    , size : ( Float, Float )
    , character : RpgCharacter

    -- , time : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Task.perform GetViewport getViewport
    )


initCharacter : RpgCharacter
initCharacter =
    { pos = ( 1000, 625 )
    , moveLeft = False
    , moveRight = False
    , moveUp = False
    , moveDown = False
    , latestDir = Right
    , faceDir = Right
    , height = 50
    , width = 50
    , speed = 500
    , move_range = ( pixelWidth, pixelHeight )
    }


initModel : Model
initModel =
    { mode = Logo
    , heroes =
        [ Hero Healer ( 6, 6 ) 100 5 5 5 False 1 -- heal 5 health
        , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
        , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
        ]
    , board = initBoard 1
    , size = ( 1500, 1000 )
    , character = initCharacter

    -- , time = 0
    }


initRPG : Model -> Model
initRPG model =
    case model.mode of
        Castle ->
            castle_1 model

        Shop ->
            shop_2 model

        Logo ->
            initModel

        _ ->
            board_1 model


initLevel : Int -> Model -> Model
initLevel k model =
    case k of
        _ ->
            board_1 model


castle_1 : Model -> Model
castle_1 model =
    { mode = Castle
    , heroes =
        [ Hero Healer ( 6, 6 ) 100 5 5 5 False 1 -- heal 5 health
        , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
        , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
        ]
    , board = initBoard 1
    , size = ( 1500, 1000 )
    , character = initCharacter

    -- , time = 0
    }


shop_2 : Model -> Model
shop_2 model =
    { mode = Shop
    , heroes =
        [ Hero Healer ( 6, 6 ) 100 5 5 5 False 1 -- heal 5 health
        , Hero Archer ( 5, 8 ) 40 20 3 5 False 2
        , Hero Assassin ( 8, 5 ) 40 20 3 6 False 3
        ]
    , board = initBoard 1
    , size = ( 1500, 1000 )
    , character = initCharacter

    -- , time = 0
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
    }
