module Message exposing (..)

import Data exposing (..)

type Msg
    = Key Dir Bool
    | Choose Pos
    | Select Class Bool
    | Resize Int Int
    | Tick Float
    | Key_None