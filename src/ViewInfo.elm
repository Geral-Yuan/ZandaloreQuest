module ViewInfo exposing (..)

import Debug exposing (toString)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (Model)

endTurnButton : Html Msg
endTurnButton =
    button
        [ style "background" "#34495f"
        , style "top" "790px"
        , style "color" "white"
        , style "font-size" "18px"
        , style "font-weight" "500"
        , style "height" "80px"
        , style "left" "1000px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "position" "absolute"
        , style "width" "160px"
        , onClick EndTurn
        ]
        [ text "End Your Turn" ]