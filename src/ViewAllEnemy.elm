module ViewAllEnemy exposing (..)

import Board exposing (Board)
import Data exposing (..)
import Debug exposing (toString)
import DetectMouse exposing (onContentMenu)
import Html exposing (Html, audio, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (Msg(..))
import Svg exposing (..)
import Svg.Attributes as SvgAttr


viewEnemy : Board -> Enemy -> Svg Msg
viewEnemy board enemy =
    let
        ( rotating, time ) =
            board.mapRotating

        ( x, y ) =
            findPos rotating board.level time enemy.pos

        class =
            toString enemy.class

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
    if not enemy.boss then
        case enemy.state of
            Attacking ->
                div
                    [ HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "top" (toString (y - 45) ++ "px")
                    , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                    ]
                    [ img [ src (fimage ++ "RedGIF.gif"), height 85, width 115 ] []
                    , audio
                        [ HtmlAttr.autoplay True
                        , HtmlAttr.loop False
                        , HtmlAttr.preload "True"
                        , HtmlAttr.src faudio
                        , HtmlAttr.id faudio
                        ]
                        []
                    ]

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
                    , onContentMenu (Hit enemy.pos)
                    ]
                    [ img [ src (fimage ++ "Red.png"), height 80, width 80 ] []
                    ]

    else
        case enemy.state of
            Attacking ->
                div
                    [ HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "top" (toString (y - 40) ++ "px")
                    , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                    , onContentMenu (Hit enemy.pos)
                    ]
                    [ img [ src "./assets/image/Boss.gif", height 80, width 115 ] []
                    ]

            Attacked _ ->
                div
                    [ HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "top" (toString (y - 40) ++ "px")
                    , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                    , onContentMenu (Hit enemy.pos)
                    ]
                    [ img [ src "./assets/image/BossGotHit.png", height 80, width 80 ] []
                    ]

            _ ->
                div
                    [ HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "top" (toString (y - 40) ++ "px")
                    , HtmlAttr.style "left" (toString (x - 40) ++ "px")
                    , onContentMenu (Hit enemy.pos)
                    ]
                    [ img [ src "./assets/image/SkullKnight.png", height 80, width 80 ] []
                    ]


viewEnemyImage : Board -> Enemy -> Svg msg
viewEnemyImage board enemy =
    let
        class =
            toString enemy.class

        minIdx =
            List.map .indexOnBoard board.enemies
                |> List.minimum
                |> Maybe.withDefault 1

        idxOnBoard =
            enemy.indexOnBoard - minIdx + 1
    in
    if not enemy.boss then
        Svg.image
            [ SvgAttr.width "70"
            , SvgAttr.height "70"
            , SvgAttr.x (toString (50 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
            , SvgAttr.y (toString (idxOnBoard * 150 - 100))
            , SvgAttr.preserveAspectRatio "none"
            , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Red.png")
            ]
            []

    else
        Svg.image
            [ SvgAttr.width "70"
            , SvgAttr.height "70"
            , SvgAttr.x (toString (50 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
            , SvgAttr.y (toString (idxOnBoard * 150 - 100))
            , SvgAttr.preserveAspectRatio "none"
            , SvgAttr.xlinkHref "./assets/image/SkullKnight.png"
            ]
            []


viewEnemyOuterFrame : Board -> Enemy -> Svg msg
viewEnemyOuterFrame board enemy =
    let
        minIdx =
            List.map .indexOnBoard board.enemies
                |> List.minimum
                |> Maybe.withDefault 1

        idxOnBoard =
            enemy.indexOnBoard - minIdx + 1
    in
    Svg.rect
        -- outer frame
        [ SvgAttr.width "420"
        , SvgAttr.height "140"
        , SvgAttr.x (toString (20 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (idxOnBoard * 150 - 130))
        , SvgAttr.fill "rgb(184,111,80)"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        ]
        []


viewEnemyInnerFrame : Board -> Enemy -> Svg msg
viewEnemyInnerFrame board enemy =
    let
        minIdx =
            List.map .indexOnBoard board.enemies
                |> List.minimum
                |> Maybe.withDefault 1

        idxOnBoard =
            enemy.indexOnBoard - minIdx + 1
    in
    Svg.rect
        -- inner frame
        [ SvgAttr.width "400"
        , SvgAttr.height "120"
        , SvgAttr.x (toString (30 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (idxOnBoard * 150 - 120))
        , SvgAttr.fill "rgb(63,40,50)"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        ]
        []


viewEnemyCondition : Board -> Enemy -> List (Svg msg)
viewEnemyCondition board enemy =
    let
        minIdx =
            List.map .indexOnBoard board.enemies
                |> List.minimum
                |> Maybe.withDefault 1

        idxOnBoard =
            enemy.indexOnBoard - minIdx + 1
    in
    [ Svg.image
        [ SvgAttr.width "30"
        , SvgAttr.height "30"
        , SvgAttr.x (toString (150 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (idxOnBoard * 150 - 100))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Heart.png"
        ]
        []
    , Svg.image
        [ SvgAttr.width "30"
        , SvgAttr.height "30"
        , SvgAttr.x (toString (150 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (idxOnBoard * 150 - 60))
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
        ( rotating, time ) =
            board.mapRotating

        ( x, y ) =
            findPos rotating board.level time enemy.pos

        healthBarlen1 =
            200 * toFloat enemy.health / toFloat enemy.maxHealth

        healthBarlen2 =
            100 * toFloat enemy.health / toFloat enemy.maxHealth

        minIdx =
            List.map .indexOnBoard board.enemies
                |> List.minimum
                |> Maybe.withDefault 1

        idxOnBoard =
            enemy.indexOnBoard - minIdx + 1
    in
    [ Svg.rect
        [ SvgAttr.width "200"
        , SvgAttr.height "20"
        , SvgAttr.x (toString (190 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (idxOnBoard * 150 - 95))
        , SvgAttr.fill "black"
        , SvgAttr.stroke "red"
        , SvgAttr.rx "5"
        ]
        []
    , Svg.rect
        [ SvgAttr.width (toString healthBarlen1)
        , SvgAttr.height "20"
        , SvgAttr.x (toString (190 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)))
        , SvgAttr.y (toString (idxOnBoard * 150 - 95))
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


viewEnemyInfo : Board -> Enemy -> List (Html Msg)
viewEnemyInfo board enemy =
    let
        minIdx =
            List.map .indexOnBoard board.enemies
                |> List.minimum
                |> Maybe.withDefault 1

        idxOnBoard =
            enemy.indexOnBoard - minIdx + 1
    in
    [ div
        [ HtmlAttr.style "top" (toString (idxOnBoard * 150 - 115) ++ "px")
        , HtmlAttr.style "left" (toString (250 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)) ++ "px")
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString enemy.health ++ "/" ++ toString enemy.maxHealth) ]
    , div
        [ HtmlAttr.style "top" (toString (idxOnBoard * 150 - 75) ++ "px")
        , HtmlAttr.style "left" (toString (200 + offsetEnemy (enemy.indexOnBoard == board.cntEnemy)) ++ "px")
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
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
