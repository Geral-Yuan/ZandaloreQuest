module ViewBoard exposing (viewBoard, viewTutorialBoard)

{-| This file fills functions related to viewing board.


# Function

@docs viewBoard, viewTutorialBoard

-}

import Html exposing (Html, audio, div)
import Html.Attributes as HtmlAttr
import Message exposing (Msg(..))
import Svg exposing (svg)
import Svg.Attributes as SvgAttr
import Type exposing (Model)
import ViewAllEnemy exposing (viewEnemy, viewEnemyInfo, viewEnemyOnboard)
import ViewAllHero exposing (viewHero, viewHeroInfo, viewHeroOnboard)
import ViewAnimation exposing (animateEnemyVisuals, animateHeroVisuals)
import ViewBoardOthers exposing (viewCrate, viewItem, viewMap, viewTurn)
import ViewConst exposing (pixelHeight, pixelWidth)
import ViewOthers exposing (..)
import ViewScenes exposing (viewBoardGameBackGround)
import ViewTutorial exposing (viewTutorialScene)
import ViewEncyclopedia exposing (viewEncyclopediaButton)


{-| This function will display the board and everything on the board.
-}
viewBoard : Model -> Html Msg
viewBoard model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "grey"
        , HtmlAttr.style "font-family" "myfont"
        ]
        ([ svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewBoardGameBackGround 100
                -- TODO: make the board game looks nicer
                :: viewMap model.board
                --++ List.map viewCoordinate model.board.map
                ++ viewHero model.board
                ++ viewEnemy model.board
                ++ List.map (viewCrate model.board) model.board.obstacles
                ++ List.concatMap (viewItem model.board) model.board.item
                ++ viewUIFrame 420 500 1570 500
                ++ viewUIButton 170 80 1700 900
                -- UI for end turn button
                ++ viewUIButton 170 80 10 750
                -- UI for skip button
                ++ viewUIButton 170 80 10 650
                -- UI for encyclopedia button
                ++ [ viewCoinSVG ( 1700, 785 ) ]
             --++ viewLines model.board
            )
         , endTurnButton
         , skipButton
         , viewCritical model.board
         , viewBoardCoin model.board
         , viewLevel model.level
         , viewTurn model
         , div
            [ HtmlAttr.style "bottom" "20px"
            , HtmlAttr.style "left" "0px"
            , HtmlAttr.style "position" "absolute"
            ]
            [ audio
                [ HtmlAttr.autoplay True
                , HtmlAttr.loop True
                , HtmlAttr.preload "True"
                , HtmlAttr.controls True
                , HtmlAttr.src "./assets/audio/BoardGameThemeSong.mp3"
                , HtmlAttr.id "BoardGameThemeSong"
                ]
                []
            ]
         ]
            ++ List.map (viewHeroOnboard model.board) (List.sortBy .indexOnBoard model.board.heroes)
            ++ List.concatMap viewHeroInfo model.board.heroes
            ++ List.map (viewEnemyOnboard model.board) (List.sortBy .indexOnBoard model.board.enemies)
            ++ List.concatMap (viewEnemyInfo model.board) model.board.enemies
            ++ List.map animateHeroVisuals model.board.heroes
            ++ List.map animateEnemyVisuals model.board.enemies
            ++ viewPopUpHint model
            ++ [viewEncyclopediaButton]
         --++ [ viewHints model.board.hintOn model.board, hintButton model.board ]
        )


{-| This function will display the tutorial board and everything on it.
-}
viewTutorialBoard : Int -> Model -> Html Msg
viewTutorialBoard k model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "grey"
        , HtmlAttr.style "font-family" "myfont"
        ]
        (svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewBoardGameBackGround 100
                :: viewMap model.board
                --++ List.map viewCoordinate model.board.map
                ++ viewHero model.board
                ++ viewEnemy model.board
                ++ List.map (viewCrate model.board) model.board.obstacles
                ++ List.concatMap (viewItem model.board) model.board.item
                ++ viewUIFrame 420 500 1570 500
                ++ viewUIButton 170 80 1700 900
                -- UI for end turn button
                ++ viewUIButton 170 80 10 750
                -- UI for skip button
                ++ viewUIButton 170 80 10 650
                -- UI for encyclopedia button
                ++ [ viewCoinSVG ( 1700, 785 ) ]
             --++ viewLines model.board
            )
            :: viewTutorialScene model k
            ++ [ endTurnButton
               , skipButton
               , viewCritical model.board
               , viewBoardCoin model.board
               , viewLevel model.level
               , viewTurn model
               ]
            ++ List.map (viewHeroOnboard model.board) (List.sortBy .indexOnBoard model.board.heroes)
            ++ List.concatMap viewHeroInfo model.board.heroes
            ++ List.map (viewEnemyOnboard model.board) (List.sortBy .indexOnBoard model.board.enemies)
            ++ List.concatMap (viewEnemyInfo model.board) model.board.enemies
            ++ List.map animateHeroVisuals model.board.heroes
            ++ List.map animateEnemyVisuals model.board.enemies
            ++ viewPopUpHint model
            ++ [viewEncyclopediaButton]
         --++ [ viewHints model.board.hintOn model.board ]
        )
