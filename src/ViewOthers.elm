module ViewOthers exposing (..)

import Board exposing (Board)
import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Svg exposing (..)
import Svg.Attributes as SvgAttr


viewCritical : Board -> Html Msg
viewCritical board =
    div
        [ HtmlAttr.style "bottom" "60px"
        , HtmlAttr.style "right" "100px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("Critical Damage: " ++ toString board.critical) ]


viewBoardCoin : Board -> Html Msg
viewBoardCoin board =
    div
        [ HtmlAttr.style "bottom" "120px"
        , HtmlAttr.style "right" "100px"
        , HtmlAttr.style "color" "orange"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("Coins: " ++ toString board.coins) ]


viewLevel : Int -> Html Msg
viewLevel level =
    let
        levelText =
            case level of
                0 ->
                    "Tutorial Level"

                k ->
                    "Level " ++ toString k
    in
    div
        [ HtmlAttr.style "bottom" "180px"
        , HtmlAttr.style "right" "100px"
        , HtmlAttr.style "color" "purple"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text levelText ]


viewCoordinate : Pos -> Svg msg
viewCoordinate ( row, column ) =
    let
        ( c_x, c_y ) =
            findPos ( row, column )

        s_row =
            toString row

        s_column =
            toString column
    in
    Svg.text_
        [ SvgAttr.x (toString c_x)
        , SvgAttr.y (toString c_y)
        , SvgAttr.textAnchor "middle"
        , SvgAttr.dominantBaseline "middle"
        , SvgAttr.fill "grey"
        ]
        [ Svg.text (s_row ++ " , " ++ s_column)
        ]


detPoints : ( Float, Float ) -> String
detPoints ( x, y ) =
    String.concat
        (List.map posToString
            [ ( x, y - 70 )
            , ( x + halfWid, y - 35 )
            , ( x + halfWid, y + 35 )
            , ( x, y + 70 )
            , ( x - halfWid, y + 35 )
            , ( x - halfWid, y - 35 )
            ]
        )


endTurnButton : Html Msg
endTurnButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "600px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "left" "1700px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "170px"
        , onClick EndTurn
        ]
        [ text "End Your Turn" ]
