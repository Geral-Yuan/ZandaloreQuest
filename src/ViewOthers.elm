module ViewOthers exposing (..)

import Board exposing (Board)
import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import ShortestPath exposing (leastWarriorPath)
import Svg exposing (..)
import Svg.Attributes as SvgAttr


viewCritical : Board -> Html Msg
viewCritical board =
    div
        [ HtmlAttr.style "bottom" "60px"
        , HtmlAttr.style "right" "-60px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("Critical Damage: " ++ toString board.critical) ]


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



{- use it to view the shortest path -}


viewRoute : List Enemy -> Board -> List Hero -> List (Svg msg)
viewRoute enemy_list board hero_list =
    let
        enemy =
            case enemy_list of
                [] ->
                    Enemy Warrior ( 3, 3 ) 100 10 5 0 True 1

                [ a ] ->
                    a

                b :: _ ->
                    b

        list_points =
            leastWarriorPath enemy board hero_list
    in
    List.map
        (\x ->
            Svg.circle
                [ SvgAttr.cx (toString (Tuple.first x))
                , SvgAttr.cy (toString (Tuple.second x))
                , SvgAttr.r "5"
                ]
                []
        )
        (List.map findPos list_points)


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
        , HtmlAttr.style "top" "790px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "left" "1000px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "160px"
        , onClick EndTurn
        ]
        [ text "End Your Turn" ]



{- viewInformation : Model -> List (Html Msg)
   viewInformation model =
       let
           hero =
               { class = Data.Warrior
               , pos = ( 6, 6 )
               , health = 100
               , damage = 15
               , armour = 5
               , energy = 5
               , selected = False
               , numberOnBoard = 1
               }
       in
       List.range 1 3
           |> List.map (getHero hero model.heroes)
           |> List.map viewHeroInfo
-}
{- getHero : Hero -> List Hero -> Int -> Hero
   getHero defaultoutput heroes n =
       case heroes of
           [] ->
               defaultoutput

           x :: xs ->
               if n == 1 then
                   x

               else
                   getHero defaultoutput xs (n - 1)
-}
