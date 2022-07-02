module Message exposing (Msg(..))

import Browser.Dom exposing (Viewport)
import Data exposing (..)


type Msg
    = Key Dir Bool
    | Choose Pos
    | Select Class Bool
    | Resize Int Int
    | Tick Float
    | Hit Bool
    | GetCritical Critical
    | EndTurn
    | Click Float Float
    | GetViewport Viewport
    | Key_None
