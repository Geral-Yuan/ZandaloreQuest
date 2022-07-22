module ViewAllHero exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (Msg(..))
import Svg exposing (..)
import Svg.Attributes as SvgAttr


viewHeroImage : Hero -> Svg msg
viewHeroImage hero =
    let
        class =
            toString hero.class
    in
    Svg.image
        [ SvgAttr.width "70"
        , SvgAttr.height "70"
        , SvgAttr.x (toString (1600 - offsetHero hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 100))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Blue.png")
        ]
        []


viewHeroFrame : Hero -> Svg msg
viewHeroFrame hero =
    Svg.rect
        [ SvgAttr.width "400"
        , SvgAttr.height "120"
        , SvgAttr.x (toString (1580 - offsetHero hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 125))
        , SvgAttr.fill "transparent"
        , SvgAttr.stroke "black"
        , SvgAttr.rx "20"
        ]
        []


viewHeroCondition : Hero -> List (Svg msg)
viewHeroCondition hero =
    [ Svg.image
        [ SvgAttr.width "30"
        , SvgAttr.height "30"
        , SvgAttr.x (toString (1700 - offsetHero hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 100))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Heart.png"
        ]
        []
    , Svg.image
        [ SvgAttr.width "30"
        , SvgAttr.height "30"
        , SvgAttr.x (toString (1700 - offsetHero hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 60))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Sword.png"
        ]
        []
    , Svg.image
        [ SvgAttr.width "30"
        , SvgAttr.height "30"
        , SvgAttr.x (toString (1830 - offsetHero hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 60))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Energy.png"
        ]
        []
    ]


viewHeroHealth : Hero -> List (Svg msg)
viewHeroHealth hero =
    let
        ( x, y ) =
            findPos hero.pos

        healthBarlen1 =
            200 * toFloat hero.health / toFloat hero.maxHealth

        healthBarlen2 =
            100 * toFloat hero.health / toFloat hero.maxHealth
    in
    [ Svg.rect
        [ SvgAttr.width "200"
        , SvgAttr.height "20"
        , SvgAttr.x (toString (1740 - offsetHero hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 95))
        , SvgAttr.fill "transparent"
        , SvgAttr.stroke "red"
        , SvgAttr.rx "5"
        ]
        []
    , Svg.rect
        [ SvgAttr.width (toString healthBarlen1)
        , SvgAttr.height "20"
        , SvgAttr.x (toString (1740 - offsetHero hero))
        , SvgAttr.y (toString (hero.indexOnBoard * 150 - 95))
        , SvgAttr.fill "red"
        , SvgAttr.stroke "red"
        , SvgAttr.rx "5"
        ]
        []
    , Svg.rect
        [ SvgAttr.width "100"
        , SvgAttr.height "10"
        , SvgAttr.x (toString (x - 50))
        , SvgAttr.y (toString (y - 60))
        , SvgAttr.fill "transparent"
        , SvgAttr.stroke "red"
        , SvgAttr.rx "5"
        ]
        []
    , Svg.rect
        [ SvgAttr.width (toString healthBarlen2)
        , SvgAttr.height "10"
        , SvgAttr.x (toString (x - 50))
        , SvgAttr.y (toString (y - 60))
        , SvgAttr.fill "red"
        , SvgAttr.stroke "red"
        , SvgAttr.rx "5"
        ]
        []
    ]


viewHeroInfo : Hero -> List (Html Msg)
viewHeroInfo hero =
    [ div
        [ HtmlAttr.style "top" (toString (hero.indexOnBoard * 150 - 115) ++ "px")
        , HtmlAttr.style "left" (toString (1800 - offsetHero hero) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString hero.health ++ "/" ++ toString hero.maxHealth) ]
    , div
        [ HtmlAttr.style "top" (toString (hero.indexOnBoard * 150 - 75) ++ "px")
        , HtmlAttr.style "left" (toString (1750 - offsetHero hero) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString hero.damage) ]
    , div
        [ HtmlAttr.style "top" (toString (hero.indexOnBoard * 150 - 75) ++ "px")
        , HtmlAttr.style "left" (toString (1880 - offsetHero hero) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString hero.energy) ]
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
