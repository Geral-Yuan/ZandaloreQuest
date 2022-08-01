module Message exposing (Msg(..))

import Browser.Dom exposing (Viewport)
import Type exposing (Class, Critical, Dir, Hero, ItemType, Pos)


type Msg
    = Key Dir Bool
    | Choose Pos
    | Enter Bool
    | Resize Int Int
    | Tick Float
    | Attack Pos Critical
    | Hit Pos
      --    | Skill Pos
    | Select Hero
    | Move Pos
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
    | DisplayUpgrade Bool
    | LuckyDraw
    | GetNewHero Class
    | EnterUpgrade
    | LevelUp ( Hero, Int )
    | ExitShop
    | ViewTutorial
    | SeeEncyclopedia
    | Back
    | RightEncyclopedia
    | LeftEncyclopedia
    | Test
    | ViewHint Bool
