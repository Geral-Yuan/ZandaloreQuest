module ViewAllEnemy exposing (..)

import Board exposing (Board)
import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (Msg(..))
import Svg exposing (..)
import Svg.Attributes as SvgAttr


viewEnemy : Enemy -> Svg Msg
viewEnemy enemy =
    let
        ( x, y ) =
            findPos enemy.pos

        class =
            toString enemy.class
    in
    case enemy.state of
        Attacking ->
            div
                [ HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "top" (toString (y - 45) ++ "px")
                , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                ]
                [ img [ src ("./assets/image/" ++ class ++ "RedGIF.gif"), height 85, width 115 ] []
                ]

        Attacked _ ->
            div
                [ HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "top" (toString (y - 40) ++ "px")
                , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                ]
                [ img [ src ("./assets/image/" ++ class ++ "GotHit.png"), height 80, width 80 ] []
                ]

        _ ->
            div
                [ HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "top" (toString (y - 40) ++ "px")
                , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                ]
                [ img [ src ("./assets/image/" ++ class ++ "Red.png"), height 80, width 80 ] []
                ]


viewEnemyImage : Board -> Enemy -> Svg msg
viewEnemyImage board enemy =
    let
        class =
            toString enemy.class
    in
    Svg.image
        [ SvgAttr.width "70"
        , SvgAttr.height "70"
        , SvgAttr.x (toString (50 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (enemy.indexOnBoard * 150 - 100))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Red.png")
        ]
        []


viewEnemyFrame : Board -> Enemy -> Svg msg
viewEnemyFrame board enemy =
    Svg.rect
        [ SvgAttr.width "400"
        , SvgAttr.height "120"
        , SvgAttr.x (toString (30 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (enemy.indexOnBoard * 150 - 125))
        , SvgAttr.fill "transparent"
        , SvgAttr.stroke "black"
        , SvgAttr.rx "20"
        ]
        []


viewEnemyCondition : Board -> Enemy -> List (Svg msg)
viewEnemyCondition board enemy =
    [ Svg.image
        [ SvgAttr.width "30"
        , SvgAttr.height "30"
        , SvgAttr.x (toString (150 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (enemy.indexOnBoard * 150 - 100))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Heart.png"
        ]
        []
    , Svg.image
        [ SvgAttr.width "30"
        , SvgAttr.height "30"
        , SvgAttr.x (toString (150 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (enemy.indexOnBoard * 150 - 60))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Sword.png"
        ]
        []

    --        , Svg.image
    --            [ SvgAttr.width "30"
    --            , SvgAttr.height "30"
    --            , SvgAttr.x (toString (280 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
    --            , SvgAttr.y (toString (enemy.indexOnBoard * 150 - 60))
    --            , SvgAttr.preserveAspectRatio "none"
    --            , SvgAttr.xlinkHref "./assets/image/Energy.png"
    --            ]
    --        []
    ]


viewEnemyHealth : Board -> Enemy -> List (Svg msg)
viewEnemyHealth board enemy =
    let
        healthBarlen =
            200 * toFloat enemy.health / toFloat enemy.maxHealth
    in
    [ Svg.rect
        [ SvgAttr.width "200"
        , SvgAttr.height "20"
        , SvgAttr.x (toString (190 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (enemy.indexOnBoard * 150 - 95))
        , SvgAttr.fill "transparent"
        , SvgAttr.stroke "red"
        , SvgAttr.rx "5"
        ]
        []
    , Svg.rect
        [ SvgAttr.width (toString healthBarlen)
        , SvgAttr.height "20"
        , SvgAttr.x (toString (190 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (enemy.indexOnBoard * 150 - 95))
        , SvgAttr.fill "red"
        , SvgAttr.stroke "red"
        , SvgAttr.rx "5"
        ]
        []
    ]


viewEnemyInfo : Board -> Enemy -> List (Html Msg)
viewEnemyInfo board enemy =
    [ div
        [ HtmlAttr.style "top" (toString (enemy.indexOnBoard * 150 - 115) ++ "px")
        , HtmlAttr.style "left" (toString (250 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString enemy.health ++ "/" ++ toString enemy.maxHealth) ]
    , div
        [ HtmlAttr.style "top" (toString (enemy.indexOnBoard * 150 - 75) ++ "px")
        , HtmlAttr.style "left" (toString (200 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString enemy.damage) ]

    --    , div
    --        [ HtmlAttr.style "top" (toString (hero.indexOnBoard * 150 - 75) ++ "px")
    --        , HtmlAttr.style "left" (toString (1880 - offsetHero hero) ++ "px")
    --        , HtmlAttr.style "color" "blue"
    --        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
    --        , HtmlAttr.style "font-size" "30px"
    --        , HtmlAttr.style "font-weight" "bold"
    --        , HtmlAttr.style "text-align" "center"
    --        , HtmlAttr.style "line-height" "60px"
    --        , HtmlAttr.style "position" "absolute"
    --        ]
    --        [ text (toString hero.energy) ]
    ]



{-
   viewEnemyInformation : List Enemy -> Int -> List (Html Msg)
   viewEnemyInformation enemies n =
       case enemies of
           [] ->
               [ div [] [] ]

           enemy :: rest ->
               viewEnemyInfo enemy n :: viewEnemyInformation rest (n + 1)
-}
{- let
       enemy =
           { class = Data.Warrior
           , pos = ( 6, 6 )
           , health = 100
           , damage = 15
           , armour = 5
           , steps = 0
           , done = False
           , indexOnBoard = 1
           }
   in
   List.range 1 3
       |> List.map (getEnemy enemy model.board.enemies)
       |> List.map viewEnemyInfo
-}


getEnemy : Enemy -> List Enemy -> Int -> Enemy
getEnemy defaultoutput enemy n =
    case enemy of
        [] ->
            defaultoutput

        x :: xs ->
            if n == 1 then
                x

            else
                getEnemy defaultoutput xs (n - 1)



{-
   viewEnemyInfo : Enemy -> Int -> Html Msg
   viewEnemyInfo enemy n =
       -- display health and energy
       let
           idx =
               toString enemy.indexOnBoard

           health =
               toString enemy.health

           ( x, y ) =
               enemy.pos

           ( xs, ys ) =
               ( toString x, toString y )
       in
       div
           [ HtmlAttr.style "top" (toString (20 + (n - 1) * 120) ++ "px")
           , HtmlAttr.style "left" "0px"
           , HtmlAttr.style "color" "blue"
           , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
           , HtmlAttr.style "font-size" "40px"
           , HtmlAttr.style "font-weight" "bold"
           , HtmlAttr.style "text-align" "center"
           , HtmlAttr.style "line-height" "60px"
           , HtmlAttr.style "position" "absolute"
           ]
           [ text ("Enemy" ++ idx ++ ": " ++ health ++ " ( " ++ xs ++ " , " ++ ys ++ " ) ") ]
-}
