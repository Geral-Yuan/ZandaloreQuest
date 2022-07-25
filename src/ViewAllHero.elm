module ViewAllHero exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import DetectMouse exposing (..)
import Html exposing (Html, audio, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Svg exposing (..)
import Svg.Attributes as SvgAttr


viewHeroImage : Hero -> List (Svg msg)
viewHeroImage hero =
    let
        class =
            toString hero.class
    in
    if hero.class == Turret then
        []

    else
        [ Svg.image
            [ SvgAttr.width "70"
            , SvgAttr.height "70"
            , SvgAttr.x (toString (1600 - offsetHero hero))
            , SvgAttr.y (toString (hero.indexOnBoard * 150 - 100))
            , SvgAttr.preserveAspectRatio "none"
            , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Blue.png")
            ]
            []
        ]


viewHeroOuterFrame : Hero -> List (Svg msg)
viewHeroOuterFrame hero =
    if hero.class == Turret then
        []

    else
        [ Svg.rect
            [ SvgAttr.width "420"
            , SvgAttr.height "140"
            , SvgAttr.x (toString (1570 - offsetHero hero))
            , SvgAttr.y (toString (hero.indexOnBoard * 150 - 130))
            , SvgAttr.fill "rgb(184,111,80)"
            , SvgAttr.stroke "black"
            , SvgAttr.strokeWidth "2"
            ]
            []
        ]


viewHeroInnerFrame : Hero -> List (Svg msg)
viewHeroInnerFrame hero =
    if hero.class == Turret then
        []

    else
        [ Svg.rect
            [ SvgAttr.width "400"
            , SvgAttr.height "120"
            , SvgAttr.x (toString (1580 - offsetHero hero))
            , SvgAttr.y (toString (hero.indexOnBoard * 150 - 120))
            , SvgAttr.fill "rgb(63,40,50)"
            , SvgAttr.stroke "black"
            , SvgAttr.strokeWidth "2"
            ]
            []
        ]


viewHeroCondition : Hero -> List (Svg msg)
viewHeroCondition hero =
    if hero.class == Turret then
        []

    else
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

        rightInfo =
            if hero.class == Turret then
                []

            else
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
                ]
    in
    rightInfo
        ++ [ Svg.rect
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
    if hero.class == Turret then
        []

    else
        [ div
            [ HtmlAttr.style "top" (toString (hero.indexOnBoard * 150 - 115) ++ "px")
            , HtmlAttr.style "left" (toString (1800 - offsetHero hero) ++ "px")
            , HtmlAttr.style "color" "white"
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
            , HtmlAttr.style "color" "white"
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
            , HtmlAttr.style "color" "white"
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

        fimage =
            "./assets/image/" ++ class

        faudio =
            "./assets/audio/"
                ++ (case class of
                        "Turret" ->
                            "Archer"

                        a ->
                            a
                   )
                ++ "SFX.mp3"
    in
    case hero.state of
        Attacking ->
            div
                [ HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "top" (toString (y - 45) ++ "px")
                , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                ]
                [ img [ src (fimage ++ "BlueGIF.gif"), height 85, width 115 ] []
                , audio
                    [ HtmlAttr.autoplay True
                    , HtmlAttr.loop False
                    , HtmlAttr.preload "True"
                    , HtmlAttr.src faudio
                    , HtmlAttr.id faudio
                    ]
                    []
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
                [ img [ src (fimage ++ "GotHit.png"), height 80, width 80 ] []
                ]

        _ ->
            div
                [ HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "top" (toString (y - 40) ++ "px")
                , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                , onClick (Select hero)
                , onContentMenu (Hit hero.pos)
                ]
                [ img [ src (fimage ++ "Blue.png"), height 80, width 80 ] []
                ]
