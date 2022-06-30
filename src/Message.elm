module Message exposing (..)

import Data exposing (..)


type Critical
    = Less
    | None
    | Low
    | Medium
    | High


type Msg
    = Key Dir Bool
    | Choose Pos
    | Select Class Bool
    | Resize Int Int
    | Tick Float
    | Hit Bool
    | GetCritical Critical
    | EndTurn
    | Key_None
