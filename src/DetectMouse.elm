module DetectMouse exposing (onContentMenu)

import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import Svg exposing (Attribute)


onContentMenu : msg -> Attribute msg
onContentMenu msg =
    preventDefaultOn "contextmenu" (Decode.map alwaysPreventDefault (Decode.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )
