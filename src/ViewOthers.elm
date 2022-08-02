module ViewOthers exposing (..)

import Data exposing (findFixedPos, posToString)
import Debug exposing (toString)
import DetectMouse exposing (onContentMenu)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr exposing (height, width)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import Type exposing (Board, FailToDo(..), Model, Pos, Scene(..))
import VectorOperation exposing (distance, neighbour, vecAdd, vecAddFloat)


dialogHelper : Float -> Float -> Float -> Float -> Float -> String -> String -> Html Msg
dialogHelper width height left top fontSize color textIn =
    div
        [ HtmlAttr.style "width" (toString width ++ "px")
        , HtmlAttr.style "height" (toString height ++ "px")
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "left" (toString left ++ "px")
        , HtmlAttr.style "top" (toString top ++ "px")
        , HtmlAttr.style "color" color
        , HtmlAttr.style "font-size" (toString fontSize ++ "px")
        ]
        [ text textIn ]


shapeHelper : ( Float, Float ) -> ( Float, Float ) -> String -> Pos -> Svg Msg
shapeHelper ( height, width ) ( x, y ) color pos =
    Svg.rect
        [ SvgAttr.stroke color
        , SvgAttr.strokeWidth "5"
        , SvgAttr.height (toString height)
        , SvgAttr.width (toString width)
        , SvgAttr.fillOpacity "0"
        , SvgAttr.x (toString (x - width / 2))
        , SvgAttr.y (toString (y - height / 2))
        , onClick (Move pos)
        , onContentMenu (Hit pos)
        ]
        []


viewCritical : Board -> Html Msg
viewCritical board =
    div
        [ HtmlAttr.style "top" "720px"
        , HtmlAttr.style "left" "1630px"
        , HtmlAttr.style "width" "400px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "left"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("Critical Damage: " ++ toString board.critical) ]


viewBoardCoin : Board -> Html Msg
viewBoardCoin board =
    div
        [ HtmlAttr.style "top" "800px"
        , HtmlAttr.style "left" "1800px"
        , HtmlAttr.style "color" "orange"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString board.coins) ]


viewCoinSVG : ( Float, Float ) -> Svg Msg
viewCoinSVG ( x, y ) =
    Svg.image
        [ SvgAttr.width "80"
        , SvgAttr.height "80"
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.xlinkHref "./assets/image/Gold.png"
        ]
        []


viewLevel : Int -> Html Msg
viewLevel level =
    let
        levelText =
            case level of
                0 ->
                    "Tutorial Level"

                6 ->
                    "Boss Level"

                k ->
                    "Level " ++ toString k
    in
    div
        [ HtmlAttr.style "top" "540px"
        , HtmlAttr.style "left" "1580px"
        , HtmlAttr.style "width" "400px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text levelText ]


viewCoordinate : Pos -> Svg msg
viewCoordinate ( row, column ) =
    let
        ( c_x, c_y ) =
            findFixedPos ( row, column )

        s_row =
            toString row

        s_column =
            toString column
    in
    Svg.text_
        [ SvgAttr.x (toString c_x)
        , SvgAttr.y (toString c_y)
        , SvgAttr.textAnchor "middle"
        , SvgAttr.dominantBaseline "middle"
        , SvgAttr.fill "grey"
        ]
        [ Svg.text (s_row ++ " , " ++ s_column)
        ]


detPoints : Board -> Pos -> ( Float, Float ) -> String
detPoints board ( row, column ) ( x, y ) =
    let
        ( rotating, time ) =
            board.mapRotating
    in
    if rotating then
        case board.level of
            5 ->
                let
                    deltaTheta =
                        if distance ( 5, 5 ) ( row, column ) == 4 then
                            -pi / 3 * time

                        else if distance ( 5, 5 ) ( row, column ) == 2 then
                            pi / 3 * time

                        else
                            0
                in
                String.concat
                    (List.map posToString
                        (List.map (vecAddFloat ( x, y ))
                            (List.map (rotateHexagonSelf ( 70, 0 ))
                                (List.map ((+) deltaTheta)
                                    [ -5 / 6 * pi, -1 / 2 * pi, -1 / 6 * pi, 1 / 6 * pi, 1 / 2 * pi, 5 / 6 * pi ]
                                )
                            )
                        )
                    )

            _ ->
                let
                    deltaTheta1 =
                        if distance ( 5, 5 ) ( row, column ) > 1 then
                            pi / 3 * time

                        else
                            0

                    deltaTheta2 =
                        if List.member ( row, column ) (List.concat (List.map (\pos -> List.map (vecAdd pos) (( 0, 0 ) :: neighbour)) [ ( 2, 5 ), ( 5, 8 ), ( 8, 2 ) ])) then
                            pi / 3 * time

                        else if List.member ( row, column ) (List.concat (List.map (\pos -> List.map (vecAdd pos) (( 0, 0 ) :: neighbour)) [ ( 5, 5 ), ( 2, 8 ), ( 8, 5 ), ( 5, 2 ) ])) then
                            -pi / 3 * time

                        else
                            0

                    deltaTheta =
                        deltaTheta1 + deltaTheta2
                in
                String.concat
                    (List.map posToString
                        (List.map (vecAddFloat ( x, y ))
                            (List.map (rotateHexagonSelf ( 70, 0 ))
                                (List.map ((+) deltaTheta)
                                    [ -5 / 6 * pi, -1 / 2 * pi, -1 / 6 * pi, 1 / 6 * pi, 1 / 2 * pi, 5 / 6 * pi ]
                                )
                            )
                        )
                    )

    else
        String.concat (List.map posToString (fixedPoints ( x, y )))


fixedPoints : ( Float, Float ) -> List ( Float, Float )
fixedPoints ( x, y ) =
    List.map (vecAddFloat ( x, y ))
        (List.map (rotateHexagonSelf ( 70, 0 ))
            [ -5 / 6 * pi, -1 / 2 * pi, -1 / 6 * pi, 1 / 6 * pi, 1 / 2 * pi, 5 / 6 * pi ]
        )


rotateHexagonSelf : ( Float, Float ) -> Float -> ( Float, Float )
rotateHexagonSelf ( x, y ) theta =
    ( x * cos theta - y * sin theta, x * sin theta + y * cos theta )


endTurnButton : Html Msg
endTurnButton =
    button
        [ HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "top" "900px"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "left" "1700px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "170px"
        , HtmlAttr.style "border" "transparent"
        , onClick EndTurn
        ]
        [ text "End Your Turn" ]


skipButton : Html Msg
skipButton =
    button
        [ HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "top" "800px"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "left" "29px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "170px"
        , HtmlAttr.style "border" "transparent"
        , onClick (Kill False)
        ]
        [ text "Skip" ]


hintButton : Board -> Html Msg
hintButton board =
    let
        on =
            not board.hintOn

        txt =
            if on then
                "Hint"

            else
                "Turn Off Hint"
    in
    button
        [ HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "top" "700px"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "height" "80px"
        , HtmlAttr.style "left" "29px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "170px"
        , HtmlAttr.style "border" "transparent"
        , onClick (ViewHint on)
        ]
        [ text txt ]


viewUIFrame : Int -> Int -> Int -> Int -> List (Svg msg)
viewUIFrame w h x y =
    -- outer
    [ Svg.rect
        [ SvgAttr.width (toString w)
        , SvgAttr.height (toString h)
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.fill "rgb(184,111,80)"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        ]
        []

    -- inner
    , Svg.rect
        [ SvgAttr.width (toString (w - 20))
        , SvgAttr.height (toString (h - 20))
        , SvgAttr.x (toString (x + 10))
        , SvgAttr.y (toString (y + 10))
        , SvgAttr.fill "rgb(63,40,50)"
        , SvgAttr.stroke "black"
        , SvgAttr.strokeWidth "2"
        ]
        []
    ]


viewUIButton : Int -> Int -> Int -> Int -> List (Svg msg)
viewUIButton w h x y =
    -- outer
    [ Svg.rect
        [ SvgAttr.width (toString w)
        , SvgAttr.height (toString h)
        , SvgAttr.x (toString x)
        , SvgAttr.y (toString y)
        , SvgAttr.fill "rgb(189,133,96)"
        , SvgAttr.stroke "rgb(61,43,31)"
        , SvgAttr.strokeWidth "2px"
        , SvgAttr.rx "10px"
        ]
        []

    -- inner
    , Svg.rect
        [ SvgAttr.width (toString (w - 4))
        , SvgAttr.height (toString (h - 10))
        , SvgAttr.x (toString (x + 2))
        , SvgAttr.y (toString (y + 2))
        , SvgAttr.fill "rgb(234,212,170)"
        , SvgAttr.rx "10px"
        ]
        []
    ]


viewPopUpHint : Model -> List (Html Msg)
viewPopUpHint model =
    let
        ( hint, time ) =
            model.popUpHint

        hintText =
            case hint of
                FailtoEnter scene ->
                    case scene of
                        ShopScene ->
                            "The door to Shop is locked"

                        DungeonScene ->
                            "The door to Side Dungeon is locked"

                        _ ->
                            "The door to Main Dungeon is locked"

                LackEnergy ->
                    "Your energy is not enough"

                FailtoTalk npc ->
                    "You have defeated " ++ npc.name

                _ ->
                    ""
    in
    if hint == Noop then
        []

    else
        [ div
            [ HtmlAttr.style "width" "800px"
            , HtmlAttr.style "height" "200px"
            , HtmlAttr.style "position" "fixed"
            , HtmlAttr.style "left" "600px"
            , HtmlAttr.style "top" (toString (500 - 100 * time) ++ "px")
            , HtmlAttr.style "color" "red"
            , HtmlAttr.style "font-size" "40px"
            , HtmlAttr.style "text-align" "center"
            , HtmlAttr.style "font-family" "myfont"
            , HtmlAttr.style "opacity" (toString (determineOpct time 2))
            ]
            [ text hintText ]
        ]


determineOpct : Float -> Float -> Float
determineOpct t tMax =
    if t <= tMax then
        sin (t * pi / tMax) * 2 / 1.732

    else
        0
