module Model exposing (..)

import Bag exposing (Bag, initBag)
import Board exposing (Board, sampleBoard)
import Browser.Dom exposing (getViewport)
import Data exposing (Class(..), Dir(..), FailToDo(..), GameMode(..), Hero, HeroState(..), NPC, Task(..), allSampleHeroes, pixelHeight, pixelWidth)
import Message exposing (Msg(..))
import NPC exposing (npcArcher, npcAssassin, npcElder, npcEngineer, npcHealer, npcMage, npcWarrior)
import RpgCharacter exposing (RpgCharacter)
import Task


type alias Model =
    { mode : GameMode
    , indexedheroes : List ( Hero, Int ) -- each hero linked to an index where 0 means not obtained so far
    , upgradePageIndex : Int
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
    , unlockShop : Bool
    , unlockDungeon : Bool
    , unlockDungeon2 : Bool
    , popUpHint : ( FailToDo, Float )
    , test : Bool

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
        allSampleHeroes |> List.filter (\( x, _ ) -> List.member x.class [ Warrior, Archer, Assassin ])
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
    , npclist = [ npcElder, npcWarrior, npcArcher, npcAssassin, npcMage, npcHealer, npcEngineer ]
    , unlockShop = False
    , unlockDungeon = False
    , unlockDungeon2 = False
    , popUpHint = ( Noop, 0 )
    , test = False

    -- , time = 0
    }
