module ViewOthers exposing (..)

import Board exposing (Board)
import Data exposing (..)
import Debug exposing (toString)
import DetectMouse exposing (..)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr exposing (height, width)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Svg exposing (..)
import Svg.Attributes as SvgAttr


dialogHelper : Float -> Float -> Float -> Float -> Float -> String -> String -> Html Msg
dialogHelper width height left top fontSize color textIn =
    div
        [ HtmlAttr.style "width" (toString width ++ "px")
        , HtmlAttr.style "height" (toString height ++ "px")
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" (toString left ++ "px")
        , HtmlAttr.style "top" (toString top ++ "px")
        , HtmlAttr.style "color" color
        , HtmlAttr.style "font-size" (toString fontSize ++ "px")
        ]
        [ text textIn ]


shapeHelper : ( Float, Float ) -> ( Float, Float ) -> String -> Pos -> Svg Msg
shapeHelper ( height, width ) ( x, y ) color pos =
    Svg.rect
        [ SvgAttr.stroke color
        , SvgAttr.strokeWidth "5"
        , SvgAttr.height (toString height)
        , SvgAttr.width (toString width)
        , SvgAttr.fillOpacity "0"
        , SvgAttr.x (toString (x - width / 2))
        , SvgAttr.y (toString (y - height / 2))
        , onClick (Move pos)
        , onContentMenu (Hit pos)
        ]
        []


viewCritical : Board -> Html Msg
viewCritical board =
    div
        [ HtmlAttr.style "top" "720px"
        , HtmlAttr.style "left" "1600px"
        , HtmlAttr.style "width" "400px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "left"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("Critical Damage: " ++ toString board.critical) ]


viewBoardCoin : Board -> Html Msg
viewBoardCoin board =
    div
        [ HtmlAttr.style "top" "800px"
        , HtmlAttr.style "left" "1800px"
        , HtmlAttr.style "color" "orange"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString board.coins) ]


viewCoinSVG : ( Float, Float ) -> Svg Msg
viewCoinSVG ( x, y ) =
    Svg.image
        [ SvgAttr.width "80"
        , SvgAttr.height "80"
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.xlinkHref "./assets/image/Gold.png"
        ]
        []


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
        [ HtmlAttr.style "top" "540px"
        , HtmlAttr.style "left" "1580px"
        , HtmlAttr.style "width" "400px"
        , HtmlAttr.style "color" "white"
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
        , HtmlAttr.style "top" "900px"
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


viewUIFrame : Int -> Int -> Int -> Int -> List (Svg msg)
viewUIFrame w h x y =
    -- outer
    [ Svg.rect
        [ SvgAttr.width (toString w)
        , SvgAttr.height (toString h)
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.fill "rgb(184,111,80)"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        ]
        []

    -- inner
    , Svg.rect
        [ SvgAttr.width (toString (w - 20))
        , SvgAttr.height (toString (h - 20))
        , SvgAttr.x (toString (x + 10))
        , SvgAttr.y (toString (y + 10))
        , SvgAttr.fill "rgb(63,40,50)"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        ]
        []
    ]
