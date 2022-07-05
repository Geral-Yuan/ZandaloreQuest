module Update exposing (update)

import Action exposing (updateAttackable, updateMoveable)
import Data exposing (..)
import HeroAttack exposing (generateDamage)
import Message exposing (Msg(..))
import Model exposing (Model)
import Random exposing (Generator)
import UpdateBoard exposing (selectHero, updateBoard)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.mode of
        BoardGame ->
            { model | board = updateBoard msg model.board |> updateAttackable |> updateMoveable }
                |> resize msg
                |> checkClick msg
                |> checkSelectedClick msg
                |> getviewport msg
                |> checkAttackClick msg
                |> randomEnemies

        _ ->
            ( model, Cmd.none )


resize : Msg -> Model -> Model
resize msg model =
    case msg of
        Resize width height ->
            { model | size = ( toFloat width, toFloat height ) }

        _ ->
            model


checkSelectedClick : Msg -> Model -> Model
checkSelectedClick msg model =
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
            if findInfoBoard clickpos > 0 then
                { model | board = selectHero model.board (findInfoBoard clickpos) }

            else
                model

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


randomEnemies : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
randomEnemies ( model, cmd ) =
    if List.length model.board.enemies == 0 && model.board.spawn > 0 then
        ( model, Cmd.batch [ Random.generate Spawn (Random.pair groupClasses (groupPositions model)), cmd ] )

    else
        ( model, cmd )


groupClasses : Generator (List Class)
groupClasses =
    Random.list 3 (Random.uniform Warrior [ Archer ])



-- Assassin, Mage, and Healer will be added later


groupPositions : Model -> Generator (List Pos)
groupPositions model =
    choosePosition model []
        |> Random.andThen
            (\pos1 ->
                choosePosition model [ pos1 ]
                    |> Random.andThen
                        (\pos2 ->
                            choosePosition model [ pos1, pos2 ]
                                |> Random.andThen (\pos3 -> choosePosition model [ pos1, pos2, pos3 ])
                                |> Random.pair (Random.constant pos2)
                        )
                    |> Random.pair (Random.constant pos1)
            )
        |> Random.map (\( x, ( y, z ) ) -> [ x, y, z ])


choosePosition : Model -> List Pos -> Generator Pos
choosePosition model list_pos =
    let
        possiblePos =
            possiblePosition model list_pos
    in
    case possiblePos of
        [] ->
            choosePosition model list_pos

        head :: rest ->
            Random.uniform head rest


possiblePosition : Model -> List Pos -> List Pos
possiblePosition model future_enemies_pos =
    let
        heroes_pos =
            List.map .pos model.board.heroes

        close_heroes =
            (( 0, 0 ) :: neighbour ++ subneighbour)
                |> cartesianProduct vecAdd heroes_pos

        close_enemy =
            (neighbour ++ subneighbour)
                |> cartesianProduct vecAdd future_enemies_pos

        possible_pos =
            listDifference (listDifference (listIntersection model.board.map close_enemy) close_heroes) (List.map .pos model.board.obstacles)
    in
    if future_enemies_pos == [] then
        listDifference (listDifference model.board.map close_heroes) (List.map .pos model.board.obstacles)

    else
        possible_pos
