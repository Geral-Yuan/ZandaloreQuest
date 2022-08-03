module ViewBoardOthers exposing (viewCrate, viewItem, viewMap, viewTurn)

{-| This file fills functions related to viewing item, obstacles and turn.


# Function

@docs viewCrate, viewItem, viewMap, viewTurn

-}

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
import ViewOthers exposing (detPoints)


{-| This function will display the board map.
-}
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


{-| This function will disiplay the turn whether it is `PlayerTurn` or `TurretTurn` or `EnemyTurn`.
-}
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


{-| This function will display crates on the board.
-}
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


{-| This function will display items on the board.
-}
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
