module ViewInfo exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (Model)



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


viewHeroInfo : Model -> Html Msg
viewHeroInfo model =
    -- display information of the selected hero
    let
        sample_hero =
            Hero Warrior ( 0, 0 ) 0 0 0 0 False 0

        hero =
            List.filter .selected model.heroes
                |> List.head
                |> Maybe.withDefault sample_hero
    in
    if hero.selected then
        div
            [ style "top" "50px"
            , style "left" "1050px"
            , style "color" "blue"
            , style "font-family" "Helvetica, Arial, sans-serif"
            , style "font-size" "50px"
            , style "font-weight" "bold"
            , style "text-align" "center"
            , style "line-height" "60px"
            , style "position" "absolute"
            ]
            [ text
                (toString hero.class
                    ++ ("\nHealth: " ++ toString hero.health)
                    ++ ("\nDamage: " ++ toString hero.damage)
                    ++ ("\nArmour: " ++ toString hero.armour)
                    ++ ("\nEnergy: " ++ toString hero.energy)
                )
            ]

    else
        div [] []


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
