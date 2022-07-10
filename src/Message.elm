module Message exposing (Msg(..))

import Browser.Dom exposing (Viewport)
import Data exposing (..)


type Msg
    = Key Dir Bool
    | Choose Pos
    | Enter Bool
      --    | Select Class Bool
    | Resize Int Int
    | Tick Float
    | Attack Pos Critical
    | EndTurn
    | Confirm
    | Click Float Float
    | Point Float Float
    | GetViewport Viewport
    | Key_None
    | SpawnEnemy ( List Class, List Pos )
    | SpawnCrate ( Pos, ItemType )
    | Kill Bool
    | Talk Bool
    | UpgradeHealth
    | UpgradeDamage
    | ExitShop
    | ViewTutorial
