module Update exposing (update)

import Action exposing (updateAttackable, updateMoveable, updateTarget)
import Browser.Dom exposing (getViewport)
import Data exposing (..)
import HeroAttack exposing (generateDamage)
import Message exposing (Msg(..))
import Model exposing (Model)
import Random exposing (Generator)
import RpgCharacter exposing (moveCharacter)
import Task
import UpdateBoard exposing (selectHero, updateBoard)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( nmodel, ncmd ) =
            case model.mode of
                BoardGame _ ->
                    { model | board = updateBoard msg model.board |> updateAttackable |> updateMoveable |> updateTarget }
                        |> checkMouseMove msg
                        |> checkSelectedClick msg
                        |> checkAttackClick msg
                        |> randomCrate msg
                        |> randomEnemies

                Logo ->
                    ( updateScene msg model, Cmd.none )

                _ ->
                    updateRPG msg model
    in
    ( nmodel
        |> resize msg
        |> getviewport msg
    , ncmd
    )



-- _ ->
--     ( model, Cmd.none )


updateCharacter : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateCharacter msg ( model, cmd ) =
    case msg of
        Tick elapse ->
            let
                newCharacter =
                    moveCharacter model.character (elapse / 1000)
            in
            if isReachable model.mode newCharacter.pos then
                ( { model | character = newCharacter }, cmd )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


isReachable : GameMode -> ( Float, Float ) -> Bool
isReachable mode ( x, y ) =
    case mode of
        Castle ->
            y > 425 && ((y > 875 && x > 0 && x < 2000) || (y <= 875 && x > 580 && x < 1420))

        _ ->
            True


updateRPG : Msg -> Model -> ( Model, Cmd Msg )
updateRPG msg model =
    let
        character =
            model.character

        ( x, y ) =
            character.pos
    in
    case msg of
        Enter False ->
            case model.mode of
                Shop ->
                    if x > 710 && x < 900 && y > 850 then
                        ( { model | mode = Castle, character = { character | width = 50, height = 50, pos = ( 1630, 890 ) } }, Task.perform GetViewport getViewport )

                    else
                        ( model, Cmd.none )

                Castle ->
                    if x > 1600 && x < 1660 && y < 900 then
                        ( { model | mode = Shop, character = { character | width = 100, height = 100, pos = ( 800, 900 ) } }, Task.perform GetViewport getViewport )

                    else if x > 950 && x < 1050 && y < 450 then
                        ( { model | mode = BoardGame 1 }, Task.perform GetViewport getViewport )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Key Left on ->
            ( { model | character = { character | moveLeft = on } }, Cmd.none )

        Key Right on ->
            ( { model | character = { character | moveRight = on } }, Cmd.none )

        Key Up on ->
            ( { model | character = { character | moveUp = on } }, Cmd.none )

        Key Down on ->
            ( { model | character = { character | moveDown = on } }, Cmd.none )

        _ ->
            ( model, Cmd.none )
                |> updateCharacter msg


updateScene : Msg -> Model -> Model
updateScene msg model =
    case msg of
        Enter False ->
            { model | mode = Castle }

        _ ->
            model


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


checkMouseMove : Msg -> Model -> Model
checkMouseMove msg model =
    let
        board =
            model.board
    in
    case msg of
        Point x y ->
            let
                ( w, h ) =
                    model.size
            in
            if w / h > pixelWidth / pixelHeight then
                { model | board = { board | pointPos = ( (x - 1 / 2) * w / h * pixelHeight + 1 / 2 * pixelWidth, y * pixelHeight * w / h ) } }

            else
                { model | board = { board | pointPos = ( x * pixelWidth, (y - 1 / 2 * h / w) * pixelWidth + 1 / 2 * pixelHeight ) } }

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
        ( model, Cmd.batch [ Random.generate SpawnEnemy (Random.pair groupEnemyClasses (groupEnemPositions model)), cmd ] )

    else
        ( model, cmd )


groupEnemyClasses : Generator (List Class)
groupEnemyClasses =
    Random.list 3 (Random.uniform Warrior [ Archer, Mage ])



-- Assassin, Mage, and Healer will be added later


groupEnemPositions : Model -> Generator (List Pos)
groupEnemPositions model =
    chooseEnemyPosition model []
        |> Random.andThen
            (\pos1 ->
                chooseEnemyPosition model [ pos1 ]
                    |> Random.andThen
                        (\pos2 ->
                            chooseEnemyPosition model [ pos1, pos2 ]
                                |> Random.andThen (\pos3 -> chooseEnemyPosition model [ pos1, pos2, pos3 ])
                                |> Random.pair (Random.constant pos2)
                        )
                    |> Random.pair (Random.constant pos1)
            )
        |> Random.map (\( x, ( y, z ) ) -> [ x, y, z ])


chooseEnemyPosition : Model -> List Pos -> Generator Pos
chooseEnemyPosition model list_pos =
    let
        possiblePos =
            possibleEnemyPosition model list_pos
    in
    case possiblePos of
        [] ->
            chooseEnemyPosition model list_pos

        head :: rest ->
            Random.uniform head rest


possibleEnemyPosition : Model -> List Pos -> List Pos
possibleEnemyPosition model future_enemies_pos =
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
            unionList [ close_heroes, List.map .pos model.board.obstacles, future_enemies_pos ]
                |> listDifference (listIntersection model.board.map close_enemy)
    in
    if future_enemies_pos == [] then
        unionList [ close_heroes, List.map .pos model.board.obstacles ]
            |> listDifference model.board.map

    else
        possible_pos


randomCrate : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
randomCrate msg ( model, cmd ) =
    case msg of
        EndTurn ->
            ( model, Cmd.batch [ cmd, Random.generate SpawnCrate (generateCrate model) ] )

        _ ->
            ( model, cmd )


generateCrate : Model -> Generator ( Pos, ItemType )
generateCrate model =
    let
        possible_pos =
            possibleCratePosition model
    in
    case possible_pos of
        [] ->
            generateCrate model

        head :: rest ->
            Random.uniform HealthPotion [ EnergyPotion, Gold ]
                |> Random.pair (Random.uniform head rest)


possibleCratePosition : Model -> List Pos
possibleCratePosition model =
    let
        heroes_pos =
            List.map .pos model.board.heroes

        enemies_pos =
            List.map .pos model.board.enemies

        close_heroes =
            (( 0, 0 ) :: neighbour)
                |> cartesianProduct vecAdd heroes_pos

        close_enemies =
            (( 0, 0 ) :: neighbour)
                |> cartesianProduct vecAdd enemies_pos
    in
    unionList [ close_heroes, close_enemies, List.map .pos model.board.obstacles, List.map .pos model.board.item ]
        |> listDifference model.board.map
