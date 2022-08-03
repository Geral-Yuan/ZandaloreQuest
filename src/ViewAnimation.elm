module ViewAnimation exposing (animateEnemyVisuals, animateHeroVisuals)

{-| This file fills functions related to viewing animation.


# Function

@docs animateEnemyVisuals, animateHeroVisuals

-}

import Data exposing (findFixedPos)
import Debug exposing (toString)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Message exposing (Msg(..))
import Svg exposing (text)
import Type exposing (Class(..), Enemy, Hero, HeroState(..))


{-| This function will display the enemies' animation
-}
animateEnemyVisuals : Enemy -> Html Msg
animateEnemyVisuals enemy =
    let
        ( x, y ) =
            findFixedPos enemy.pos
    in
    div
        [ HtmlAttr.style "left" (toString x ++ "px")
        , HtmlAttr.style "top" (toString (y - 80) ++ "px")
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "60px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ case enemy.state of
            Attacked k ->
                text ("-" ++ toString k)

            GettingHealed h2 ->
                text ("+" ++ toString h2)

            _ ->
                text ""
        ]


{-| This function will display the heroes' animation
-}
animateHeroVisuals : Hero -> Html Msg
animateHeroVisuals hero =
    let
        ( x, y ) =
            findFixedPos hero.pos
    in
    div
        [ HtmlAttr.style "left" (toString (x + 40) ++ "px")
        , HtmlAttr.style "top" (toString (y - 80) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "60px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ case hero.state of
            Moving ->
                text "-2 Energy"

            TakingEnergy ->
                text "Max Energy reached"

            TakingHealth h1 ->
                text ("+" ++ toString h1)

            GettingHealed h2 ->
                text ("+" ++ toString h2)

            Attacking ->
                if hero.class /= Turret then
                    text "-3 Energy"

                else
                    text ""

            Attacked k ->
                text ("-" ++ toString k)

            _ ->
                text ""
        ]
