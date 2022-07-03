module Update exposing (update)

import Board exposing (Board)
import Data exposing (..)
import HeroAttack exposing (generateDamage, selectedHero)
import Message exposing (Msg(..))
import Model exposing (Model)
import UpdateBoard exposing (updateBoard)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.mode of
        BoardGame ->
            { model | board = updateBoard msg model.board |> updateAttackable |> updateMoveable }
                |> resize msg
                |> checkClick msg
                |> getviewport msg
                |> checkAttackClick msg

        _ ->
            ( model, Cmd.none )


updateAttackable : Board -> Board
updateAttackable board =
    case selectedHero board.heroes of
        Nothing ->
            { board | attackable = [] }

        Just hero ->
            let
                realattackRange =
                    List.map (vecAdd hero.pos) (attackRange board hero)

                can_attack =
                    listintersection realattackRange (List.map .pos board.obstacles ++ List.map .pos board.enemies)
            in
            { board | attackable = can_attack }


attackRange : Board -> Hero -> List Pos
attackRange board hero =
    case hero.class of
        Archer ->
            List.concat (List.map (stuckInWay board hero.pos) neighbour)

        Mage ->
            subneighbour

        _ ->
            neighbour


stuckInWay : Board -> Pos -> Pos -> List Pos
stuckInWay board heropos pos =
    let
        linePos =
            List.map (vecAdd heropos) (sameline pos)

        inWay =
            listintersection linePos (List.map .pos board.obstacles ++ List.map .pos board.enemies)
    in
    case leastdistance inWay heropos of
        Nothing ->
            sameline pos

        Just dis ->
            List.map (\k -> vecScale k pos) (List.range 1 dis)


updateMoveable : Board -> Board
updateMoveable board =
    case selectedHero board.heroes of
        Nothing ->
            { board | moveable = [] }

        Just hero ->
            let
                realmoveRange =
                    List.map (vecAdd hero.pos) neighbour

                can_move =
                    List.filter (\pos -> not (List.member pos (List.map .pos board.obstacles ++ List.map .pos board.enemies ++ List.map .pos board.heroes))) realmoveRange
            in
            { board | moveable = can_move }


resize : Msg -> Model -> Model
resize msg model =
    case msg of
        Resize width height ->
            { model | size = ( toFloat width, toFloat height ) }

        _ ->
            model


checkAttackClick : Msg -> Model -> ( Model, Cmd Msg )
checkAttackClick msg model =
    case msg of
        Click x y ->
            let
                ( w, h ) =
                    model.size

                clickpos =
                    if w / h > pixelWidth / pixelHeight then
                        ( (x - 1 / 2) * w / h * pixelHeight + 1 / 2 * pixelWidth, y * pixelHeight * w / h )

                    else
                        ( x * pixelWidth, (y - 1 / 2 * h / w) * pixelWidth + 1 / 2 * pixelHeight )
            in
            case findHexagon clickpos of
                Just cell ->
                    ( model, generateDamage cell )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


checkClick : Msg -> Model -> Model
checkClick msg model =
    case msg of
        Click x y ->
            let
                ( w, h ) =
                    model.size
            in
            if w / h > pixelWidth / pixelHeight then
                { model | clickPos = ( (x - 1 / 2) * w / h * pixelHeight + 1 / 2 * pixelWidth, y * pixelHeight * w / h ) }

            else
                { model | clickPos = ( x * pixelWidth, (y - 1 / 2 * h / w) * pixelWidth + 1 / 2 * pixelHeight ) }

        _ ->
            model


getviewport : Msg -> Model -> Model
getviewport msg model =
    case msg of
        GetViewport { viewport } ->
            { model
                | size =
                    ( viewport.width
                    , viewport.height
                    )
            }

        _ ->
            model
