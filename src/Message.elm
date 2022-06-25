module Message exposing (..)

import Data exposing (..)

type Msg
    = Key Dir
    | Choose Pos
    | Resize Int Int
    | Tick Float