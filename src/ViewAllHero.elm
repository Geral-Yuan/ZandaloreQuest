module ViewAllHero exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (Msg(..))
import Svg exposing (..)
import Svg.Attributes as SvgAttr


viewHeroInfo1 : Hero -> Svg msg
viewHeroInfo1 hero =
    let
        class =
            toString hero.class
    in
    Svg.image
        [ SvgAttr.width "70"
        , SvgAttr.height "70"
        , SvgAttr.x (toString (1600 - offset hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 100))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Blue.png")
        ]
        []


viewHeroInfo2 : Hero -> Svg msg
viewHeroInfo2 hero =
    Svg.rect
        [ SvgAttr.width "400"
        , SvgAttr.height "120"
        , SvgAttr.x (toString (1580 - offset hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 125))
        , SvgAttr.fill "transparent"
        , SvgAttr.stroke "black"
        , SvgAttr.rx "20"
        ]
        []


viewHeroInfo3 : Hero -> Html Msg
viewHeroInfo3 hero =
    div
        [ HtmlAttr.style "top" (toString (hero.indexOnBoard * 150 - 115) ++ "px")
        , HtmlAttr.style "left" (toString (1700 - offset hero) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "20px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text
            (("Health: " ++ toString hero.health)
                ++ (" Damage: " ++ toString hero.damage)
            )
        ]


viewHeroInfo4 : Hero -> Html Msg
viewHeroInfo4 hero =
    div
        [ HtmlAttr.style "top" (toString (hero.indexOnBoard * 150 - 75) ++ "px")
        , HtmlAttr.style "left" (toString (1700 - offset hero) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "20px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text
            (" Energy: " ++ toString hero.energy)
        ]


viewHero : Hero -> Html Msg
viewHero hero =
    let
        ( x, y ) =
            findPos hero.pos

        class =
            toString hero.class
    in
    case hero.state of
        Attacking ->
            div
                [ HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "top" (toString (y - 45) ++ "px")
                , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                ]
                [ img [ src ("./assets/image/" ++ class ++ "BlueGIF.gif"), height 85, width 115 ] []
                ]

        -- Svg.image
        --     [ SvgAttr.width "80"
        --     , SvgAttr.height "80"
        --     , SvgAttr.x (toString (x - 40))
        --     , SvgAttr.y (toString (y - 40))
        --     , SvgAttr.preserveAspectRatio "none"
        --     , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "BlueGIF.gif")
        --     ]
        --     []
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
                [ img [ src ("./assets/image/" ++ class ++ "Blue.png"), height 80, width 80 ] []
                ]



-- Svg.image
--     [ SvgAttr.width "80"
--     , SvgAttr.height "80"
--     , SvgAttr.x (toString (x - 40))
--     , SvgAttr.y (toString (y - 40))
--     , SvgAttr.preserveAspectRatio "none"
--     , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Blue.png")
--     ]
--     []
