module DetectMouse exposing (onContentMenu)

{-| This file fills functions related to preventing content menu from popping up.


# Functions

@docs onContentMenu

-}

import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import Svg exposing (Attribute)


{-| This function will prevent content menu from popping up when right click on the mouse.
-}
onContentMenu : msg -> Attribute msg
onContentMenu msg =
    preventDefaultOn "contextmenu" (Decode.map alwaysPreventDefault (Decode.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )
