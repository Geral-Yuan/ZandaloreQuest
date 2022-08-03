module Model exposing (init)

{-| This file fills functions related to model.


# Function

@docs init

-}

import Bag exposing (initBag)
import Board exposing (sampleBoard)
import Browser.Dom exposing (getViewport)
import Data exposing (allSampleHeroes)
import Message exposing (Msg(..))
import NPC exposing (npcMap)
import Task exposing (perform)
import Type exposing (BoardState(..), Class(..), Dir(..), FailToDo(..), GameMode(..), Model, RpgCharacter, Task(..))
import ViewConst exposing (pixelHeight, pixelWidth)


{-| This function will return the initial state of the model.
-}
init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , perform GetViewport getViewport
    )


initCharacter : RpgCharacter
initCharacter =
    { pos = ( 1005, 555 )
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


initModel : Model
initModel =
    { mode = Logo
    , indexedheroes =
        allSampleHeroes |> List.filter (\( x, _ ) -> List.member x.class [ Warrior, Archer ])
    , upgradePageIndex = 1
    , board = sampleBoard
    , size = ( 1500, 1000 )
    , character = initCharacter
    , chosenHero = []
    , bag = initBag
    , previousMode = BoardGame
    , level = 0
    , time = 0
    , cntTask = MeetElder
    , npclist = List.map npcMap (List.range 1 7)
    , unlockShop = False
    , unlockDungeon = False
    , unlockDungeon2 = False
    , popUpHint = ( Noop, 0 )
    , test = False
    , isDisplayUpgrade = False
    }
