module Message exposing (..)

import Data exposing (..)

type Msg
    = Key Dir Bool
    | Choose Pos
    | Resize Int Int
    | Tick Float
    | Key_None