module Model exposing (..)

import Bag exposing (Bag, initBag)
import Board exposing (Board, sampleBoard)
import Browser.Dom exposing (getViewport)
import Data exposing (..)
import Message exposing (Msg(..))
import RpgCharacter exposing (..)
import Task
import NPC exposing (npcElder)
import NPC exposing (npcWarrior)
import NPC exposing (npcArcher)
import NPC exposing (npcMage)
import NPC exposing (npcHealer)
import NPC exposing (npcEngineer)


type alias Model =
    { mode : GameMode
    , indexedheroes : List ( Hero, Int ) -- each hero linked to an index where 0 means not obtained so far
    , board : Board
    , size : ( Float, Float )
    , character : RpgCharacter
    , chosenHero : List Int
    , bag : Bag
    , previousMode : GameMode
    , level : Int
    , time : Float
    , cntTask : Task
    , npclist : List NPC 

    -- , time : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Task.perform GetViewport getViewport
    )


initCharacter : RpgCharacter
initCharacter =
    { pos = ( 1005, 555 )

    --    , state = Still
    , moveLeft = False
    , moveRight = False
    , moveUp = False
    , moveDown = False
    , faceDir = Right
    , height = 64
    , width = 64
    , speed = 500
    , move_range = ( pixelWidth, pixelHeight )
    }



-- Hero Healer ( 6, 6 ) 100 5 5 5 False 1 -- heal 5 health


initModel : Model
initModel =
    { mode = Logo
    , indexedheroes =
        [ ( Hero Warrior ( 0, 0 ) 80 80 15 5 False Waiting 0, 1 )
        , ( Hero Archer ( 0, 0 ) 40 40 20 5 False Waiting 0, 2 )
        , ( Hero Assassin ( 0, 0 ) 40 40 20 6 False Waiting 0, 3 )
        , ( Hero Mage ( 0, 0 ) 50 50 12 3 False Waiting 0, 4 )
        , ( Hero Healer ( 0, 0 ) 50 50 5 5 False Waiting 0, 5 )
        , ( Hero Engineer ( 0, 0 ) 50 50 5 5 False Waiting 0, 6 )
        ]
    , board = sampleBoard
    , size = ( 1500, 1000 )
    , character = initCharacter
    , chosenHero = []
    , bag = initBag
    , previousMode = BoardGame
    , level = 1
    , time = 0
    , cntTask = MeetElder
    , npclist = [npcElder, npcWarrior, npcArcher, npcMage, npcHealer, npcEngineer]

    -- , time = 0
    }



{-
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
       , indexedheroes =
           [ ( Hero Warrior ( 0, 0 ) 50 15 5 3 False 0, 1 )
           , ( Hero Archer ( 0, 0 ) 40 20 3 5 False 0, 2 )
           , ( Hero Assassin ( 0, 0 ) 40 20 3 6 False 0, 3 )
           , ( Hero Mage ( 0, 0 ) 50 15 5 3 False 0, 4 )
           ]
       , board = sampleBoard
       , size = ( 1500, 1000 )
       , character = initCharacter
       , chosenHero = []
       -- , time = 0
       }


   shop_2 : Model -> Model
   shop_2 model =
       { mode = Shop
       , indexedheroes =
           [ ( Hero Warrior ( 0, 0 ) 50 15 5 3 False 0, 1 )
           , ( Hero Archer ( 0, 0 ) 40 20 3 5 False 0, 2 )
           , ( Hero Assassin ( 0, 0 ) 40 20 3 6 False 0, 3 )
           , ( Hero Mage ( 0, 0 ) 50 15 5 3 False 0, 4 )
           ]
       , board = sampleBoard
       , size = ( 1500, 1000 )
       , character = initCharacter
       , chosenHero = []
       -- , time = 0
       }


   board_1 : Model -> Model
   board_1 model =
       { mode = BoardGame 1
       , indexedheroes =
           [ ( Hero Warrior ( 0, 0 ) 50 15 5 3 False 0, 1 )
           , ( Hero Archer ( 0, 0 ) 40 20 3 5 False 0, 2 )
           , ( Hero Assassin ( 0, 0 ) 40 20 3 6 False 0, 3 )
           , ( Hero Mage ( 0, 0 ) 50 15 5 3 False 0, 4 )
           ]
       , board = sampleBoard
       , size = ( 1500, 1000 )
       , character = initCharacter
       , chosenHero = []
       -- , time = 0
       }
-}
