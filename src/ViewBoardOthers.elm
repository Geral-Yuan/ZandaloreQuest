module ViewBoardOthers exposing (viewMap, viewItem, viewCrate, viewTurn)

import Data exposing (findPos)
import Debug exposing (toString)
import DetectMouse exposing (onContentMenu)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import ListOperation exposing (listUnion)
import Message exposing (Msg(..))
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import Type exposing (Board, BoardState(..), Item, ItemType(..), Model, Obstacle, ObstacleType(..), Pos, Turn(..))
import ViewOthers exposing (detPoints, shapeHelper, dialogHelper)
import ViewTutorial exposing (viewHintBackground)


viewMap : Board -> List (Svg Msg)
viewMap board =
    List.map (viewCell board) board.map


viewCell : Board -> Pos -> Svg Msg
viewCell board pos =
    let
        ( rotating, time ) =
            board.mapRotating
    in
    if List.member pos (List.map .pos (List.filter (\obstacle -> obstacle.obstacleType == Unbreakable) board.obstacles)) then
        Svg.polygon
            [ SvgAttr.fill "black"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints board pos (findPos rotating board.level time pos))
            , onContentMenu (Hit pos)
            ]
            []

    else if List.member pos board.target && board.boardState == NoActions then
        if List.member pos board.skillable then
            Svg.polygon
                [ SvgAttr.fill "rgb(154,205,50)"
                , SvgAttr.stroke "blue"
                , SvgAttr.points (detPoints board pos (findPos rotating board.level time pos))
                , onClick (Move pos)
                , onContentMenu (Hit pos)
                ]
                []

        else if List.member pos (List.map .pos board.enemies ++ List.map .pos board.obstacles) then
            Svg.polygon
                [ SvgAttr.fill "yellow"
                , SvgAttr.stroke "blue"
                , SvgAttr.points (detPoints board pos (findPos rotating board.level time pos))
                , onContentMenu (Hit pos)
                ]
                []

        else
            Svg.polygon
                [ SvgAttr.fill "rgb(132,112,255)"
                , SvgAttr.stroke "blue"
                , SvgAttr.points (detPoints board pos (findPos rotating board.level time pos))
                , onClick (Move pos)
                , onContentMenu (Hit pos)
                ]
                []

    else if List.member pos (listUnion board.attackable board.skillable ++ board.enemyAttackable) && board.boardState == NoActions then
        Svg.polygon
            [ SvgAttr.fill "rgb(173,216,230)"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints board pos (findPos rotating board.level time pos))
            , onContentMenu (Hit pos)
            ]
            []

    else
        Svg.polygon
            [ SvgAttr.fill "white"
            , SvgAttr.stroke "blue"
            , SvgAttr.points (detPoints board pos (findPos rotating board.level time pos))
            , SvgAttr.opacity "75%"
            , onClick (Move pos)
            , onContentMenu (Hit pos)
            ]
            []


viewTurn : Model -> Html Msg
viewTurn model =
    div
        [ HtmlAttr.style "left" "1580px"
        , HtmlAttr.style "top" "640px"
        , HtmlAttr.style "width" "400px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ case model.board.turn of
            EnemyTurn ->
                text "Enemy Turn"

            TurretTurn ->
                text "Turret Turn"

            _ ->
                text "Your Turn"
        ]


viewCrate : Board -> Obstacle -> Svg Msg
viewCrate board obs =
    let
        ( rotating, time ) =
            board.mapRotating

        ( x, y ) =
            findPos rotating board.level time obs.pos
    in
    if obs.obstacleType == MysteryBox then
        Svg.image
            [ SvgAttr.width "80"
            , SvgAttr.height "80"
            , SvgAttr.x (toString (x - 40))
            , SvgAttr.y (toString (y - 40))
            , SvgAttr.preserveAspectRatio "none"
            , SvgAttr.xlinkHref "./assets/image/Crate.png"
            , onContentMenu (Hit obs.pos)
            ]
            []

    else
        -- TODO: if possible, return nothing
        Svg.rect [] []


viewItem : Board -> Item -> List (Svg Msg)
viewItem board item =
    let
        ( rotating, time ) =
            board.mapRotating

        ( x, y ) =
            findPos rotating board.level time item.pos

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
                , onClick (Move item.pos)
                , onContentMenu (Hit item.pos)
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
                , onClick (Move item.pos)
                , onContentMenu (Hit item.pos)
                ]
                []
            ]


viewHints : Bool -> Board -> Html Msg
viewHints bool board =
    let
        ( rotating, time ) =
            board.mapRotating

        enemiesPos =
            List.map (\enemy -> enemy.pos) board.enemies

        enemyPos =
            Maybe.withDefault ( 0, 0 ) (List.head enemiesPos)

        ( ex, ey ) =
            findPos rotating board.level time enemyPos

        heroesPos =
            List.map (\hero -> hero.pos) board.heroes

        heroPos =
            Maybe.withDefault ( 0, 0 ) (List.head heroesPos)

        ( hx, hy ) =
            findPos rotating board.level time heroPos
    in
    if bool == True then
        div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "0"
            , HtmlAttr.style "top" "0"
            , HtmlAttr.style "font-family" "myfont"
            ]
            [ Svg.svg
                [ SvgAttr.width "100%"
                , SvgAttr.height "100%"
                ]
                [ shapeHelper ( 100, 100 ) ( hx, hy + 5 ) "blue" ( 0, 0 )
                , shapeHelper ( 100, 100 ) ( ex, ey + 5 ) "red" ( 0, 0 )
                , viewHintBackground 400 50 (hx + 50) (hy - 50)
                , viewHintBackground 500 50 (hx + 50) (hy + 50)
                , viewHintBackground 450 50 (ex + 50) (ey - 50)
                , viewHintBackground 350 50 210 680
                ]
            , dialogHelper 400 20 (hx + 50) (hy - 50) 30 "white" "1. Left click on a hero to select"
            , dialogHelper 500 20 (hx + 50) (hy + 50) 30 "white" "2. Left click on blue hexagons to move"
            , dialogHelper 450 20 (ex + 50) (ey - 50) 30 "white" "3. Right click on enemies to attack"
            , dialogHelper 350 20 210 680 30 "white" "4. Click to turn of the hint"
            ]

    else
        div [] []
