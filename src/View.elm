module View exposing (view)

import Board exposing (Board)
import Data exposing (..)
import Debug exposing (toString)
import DetectMouse exposing (..)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import ViewAllEnemy exposing (..)
import ViewAllHero exposing (..)
import ViewChoose exposing (viewHeroChoose, viewShopChoose)
import ViewOthers exposing (..)
import ViewScenes exposing (..)
import ViewTutorial exposing (..)


view : Model -> Html Msg
view model =
    let
        viewAll =
            case model.mode of
                Logo ->
                    viewScene0 model

                BoardGame ->
                    viewBoard model

                Castle ->
                    viewCastle model

                Shop ->
                    viewShop model

                HeroChoose ->
                    viewHeroChoose model

                BuyingItems ->
                    viewShopChoose model

                Dungeon ->
                    viewDungeon model

                Dungeon2 ->
                    viewDungeon2 model

                Tutorial k ->
                    viewTutorial k model

                Dialog task ->
                    viewDialog task model
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "background" "black"
        ]
        [ viewAll
        ]


viewDialog : Task -> Model -> Html Msg
viewDialog task model =
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
        , HtmlAttr.style "background" "black"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewDungeonSvg
            , viewDialogBox
            ]
        , viewDialogMatch task
        ]


viewDialogMatch : Task -> Html Msg
viewDialogMatch task =
    case task of
        MeetElder ->
            viewDialogElder

        _ ->
            viewDialogGeneral


viewDialogElder : Html Msg
viewDialogElder =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "100px"
            , HtmlAttr.style "left" "350px"
            ]
            [ img [ src "./assets/image/MainCharacter.png", height 400, width 480 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "100px"
            , HtmlAttr.style "left" "1180px"
            , HtmlAttr.style "transform" "scaleX(-1)"
            ]
            [ img [ src "./assets/image/EvilNPC.png", height 400, width 480 ] []
            ]
        , div
            [ HtmlAttr.style "width" "1300px"
            , HtmlAttr.style "height" "450px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "370px"
            , HtmlAttr.style "top" "560px"
            , HtmlAttr.style "color" "blue"
            , HtmlAttr.style "font-size" "50px"
            ]
            [ text "Elder: Welcome to the tutorial! The warrior and archer will help you on this arduous journey, choose one more hero to join you in this tutorial. Click Enter to countinue." ]
        ]


viewDialogGeneral : Html Msg
viewDialogGeneral =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "50px"
            , HtmlAttr.style "left" "50px"
            ]
            [ img [ src "./assets/image/MainCharacter.png", height 300, width 380 ] []
            ]
        ]


viewDialogBox : Svg Msg
viewDialogBox =
    Svg.image
        [ SvgAttr.width "1500"
        , SvgAttr.height "500"
        , SvgAttr.x (toString (pixelWidth / 2 - 750))
        , SvgAttr.y (toString (pixelHeight / 2))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/DialogBox.png"
        ]
        []


viewTutorial : Int -> Model -> Html Msg
viewTutorial k model =
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
        ]
        ([ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewMap model.board
--                ++ List.map viewCoordinate model.board.map
                ++ List.map viewHeroImage model.board.heroes
                ++ List.map viewHeroFrame model.board.heroes
                ++ List.concat (List.map viewHeroCondition model.board.heroes)
                ++ List.concat (List.map viewHeroHealth model.board.heroes)
                ++ List.map (viewEnemyImage model.board) model.board.enemies
                ++ List.map (viewEnemyFrame model.board) model.board.enemies
                ++ List.concat (List.map (viewEnemyCondition model.board) model.board.enemies)
                ++ List.concat (List.map (viewEnemyHealth model.board) model.board.enemies)
                ++ List.map viewCrate model.board.obstacles
                ++ List.concatMap viewItem model.board.item
             --++ viewLines model.board
            )
         , endTurnButton
         , viewCritical model.board
         , viewBoardCoin model.board
         , viewLevel model.level
         , viewTurn model
         , viewTutorialScene k model

         --  , viewClickPosition model
         --  , viewTips
         ]
            ++ List.map viewHero (List.sortBy .indexOnBoard model.board.heroes)
            ++ List.concat (List.map viewHeroInfo model.board.heroes)
            ++ List.map viewEnemy (List.sortBy .indexOnBoard model.board.enemies)
            ++ List.concat (List.map (viewEnemyInfo model.board) model.board.enemies)
            ++ List.map animateHeroVisuals model.board.heroes
            ++ List.map animateEnemyVisuals model.board.enemies
        )


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
        ]
        ([ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewMap model.board
--                ++ List.map viewCoordinate model.board.map
                ++ List.map viewHeroImage model.board.heroes
                ++ List.map viewHeroFrame model.board.heroes
                ++ List.concat (List.map viewHeroCondition model.board.heroes)
                ++ List.concat (List.map viewHeroHealth model.board.heroes)
                ++ List.map (viewEnemyImage model.board) model.board.enemies
                ++ List.map (viewEnemyFrame model.board) model.board.enemies
                ++ List.concat (List.map (viewEnemyCondition model.board) model.board.enemies)
                ++ List.concat (List.map (viewEnemyHealth model.board) model.board.enemies)
                ++ List.map viewCrate model.board.obstacles
                ++ List.concatMap viewItem model.board.item
             --++ viewLines model.board
            )
         , endTurnButton
         , viewCritical model.board
         , viewBoardCoin model.board
         , viewLevel model.level
         , viewTurn model

         --  , viewClickPosition model
         --  , viewTips
         ]
            ++ List.map viewHero (List.sortBy .indexOnBoard model.board.heroes)
            ++ List.concat (List.map viewHeroInfo model.board.heroes)
            ++ List.map viewEnemy (List.sortBy .indexOnBoard model.board.enemies)
            ++ List.concat (List.map (viewEnemyInfo model.board) model.board.enemies)
            ++ List.map animateHeroVisuals model.board.heroes
            ++ List.map animateEnemyVisuals model.board.enemies
        )


animateEnemyVisuals : Enemy -> Html Msg
animateEnemyVisuals enemy =
    let
        ( x, y ) =
            findPos enemy.pos
    in
    div
        [ HtmlAttr.style "left" (toString x ++ "px")
        , HtmlAttr.style "top" (toString (y - 80) ++ "px")
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
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


animateHeroVisuals : Hero -> Html Msg
animateHeroVisuals hero =
    let
        ( x, y ) =
            findPos hero.pos
    in
    div
        [ HtmlAttr.style "left" (toString (x + 40) ++ "px")
        , HtmlAttr.style "top" (toString (y - 80) ++ "px")
        , HtmlAttr.style "color" "blue"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
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
                text "+2 Energy"

            TakingHealth h1 ->
                text ("+" ++ toString h1)

            GettingHealed h2 ->
                text ("+" ++ toString h2)

            Attacking ->
                text "-3 Energy"

            Attacked k ->
                text ("-" ++ toString k)

            _ ->
                text ""
        ]



-- view where the enermy archer can attack the hero (for debugging)
{-
   viewLines :  Board -> List (Svg msg)
   viewLines board  =
       let

           list_points =
               --List.concatMap (\x -> leastArcherPath x board) board.enemies
                getHeroesLines board
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
-}


viewMap : Board -> List (Svg Msg)
viewMap board =
    List.map (viewCell board) board.map


viewCell : Board -> Pos -> Svg Msg
viewCell board pos =
    if List.member pos (List.map .pos (List.filter (\obstacle -> obstacle.obstacleType == Unbreakable) board.obstacles)) then
        Svg.polygon
            [ SvgAttr.fill "black"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints (findPos pos))
            ]
            []

    else if List.member pos board.target then
        if List.member pos board.skillable then
            Svg.polygon
                [ SvgAttr.fill "rgb(154,205,50)"
                , SvgAttr.stroke "blue"
                , SvgAttr.points (detPoints (findPos pos))
                , onClick (Move pos)
                ]
                []

        else
            Svg.polygon
                [ SvgAttr.fill "yellow"
                , SvgAttr.stroke "blue"
                , SvgAttr.points (detPoints (findPos pos))
                , onClick (Move pos)
                ]
                []

    else if List.member pos (listUnion board.attackable board.skillable ++ board.enemyAttackable) then
        Svg.polygon
            [ SvgAttr.fill "rgb(173,216,230)"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints (findPos pos))
            ]
            []

    else
        Svg.polygon
            [ SvgAttr.fill "white"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints (findPos pos))
            , onClick (Move pos)
            ]
            []


viewTurn : Model -> Html Msg
viewTurn model =
    div
        [ HtmlAttr.style "left" "1690px"
        , HtmlAttr.style "top" "500px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ case model.board.turn of
            EnemyTurn ->
                text "Enemy Turn"

            _ ->
                text "Your Turn"
        ]


viewCrate : Obstacle -> Svg Msg
viewCrate obs =
    let
        ( x, y ) =
            findPos obs.pos
    in
    if obs.obstacleType == MysteryBox then
        Svg.image
            [ SvgAttr.width "80"
            , SvgAttr.height "80"
            , SvgAttr.x (toString (x - 40))
            , SvgAttr.y (toString (y - 40))
            , SvgAttr.preserveAspectRatio "none"
            , SvgAttr.xlinkHref "./assets/image/Crate.png"
            ]
            []

    else
        -- TODO: if possible, return nothing
        Svg.rect [] []


viewItem : Item -> List (Svg Msg)
viewItem item =
    let
        ( x, y ) =
            findPos item.pos

        itemtype =
            toString item.itemType
    in
    case item.itemType of
        Gold _ ->
            [ Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 40))
                , SvgAttr.y (toString (y - 40))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "./assets/image/Gold.png"
                ]
                []
            ]

        NoItem ->
            []

        _ ->
            [ Svg.image
                [ SvgAttr.width "80"
                , SvgAttr.height "80"
                , SvgAttr.x (toString (x - 40))
                , SvgAttr.y (toString (y - 40))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref ("./assets/image/" ++ itemtype ++ ".png")
                ]
                []
            ]
