module UpdateSpawn exposing (randomCrate, randomEnemies, spawnCrate, spawnEnemies)

{-| This file fills functions related to generating random obstacles and enemies in the board game mode.


# Function

@docs randomCrate, randomEnemies, spawnCrate, spawnEnemies

-}

import Data exposing (sampleEnemy)
import ListOperation exposing (cartesianProduct, listDifference, unionList)
import Message exposing (Msg(..))
import Random exposing (Generator)
import Type exposing (Board, Class(..), Hero, ItemType(..), Model, Obstacle, ObstacleType(..), Pos, Turn(..))
import VectorOperation exposing (neighbour, subneighbour, vecAdd)


{-| This function will spawn the generated enemies.
-}
spawnEnemies : List Class -> List Pos -> Board -> Board
spawnEnemies list_class list_pos board =
    if List.length board.enemies == 0 && board.spawn > 0 then
        let
            n_enemies =
                List.range (board.index + 1) (board.index + 3)
                    |> List.map3 sampleEnemy list_class list_pos
        in
        { board | enemies = n_enemies, spawn = board.spawn - 1 }

    else
        board


{-| This function will spawn the generated crate.
-}
spawnCrate : Pos -> ItemType -> Board -> Board
spawnCrate pos itype board =
    let
        crate =
            Obstacle MysteryBox pos itype

        nobstacles =
            crate :: board.obstacles
    in
    { board | obstacles = nobstacles }


{-| This function will generate group of enemies at random position and class.
-}
randomEnemies : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
randomEnemies ( model, cmd ) =
    if List.length model.board.enemies == 0 && model.board.spawn > 0 then
        ( model, Cmd.batch [ Random.generate SpawnEnemy (Random.pair groupEnemyClasses (groupEnemPositions model)), cmd ] )

    else
        ( model, cmd )


groupEnemyClasses : Generator (List Class)
groupEnemyClasses =
    Random.list 3 (Random.uniform Warrior [ Archer, Mage, Assassin, Healer ])


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

        -- close_enemy =
        --     (neighbour ++ subneighbour)
        --         |> cartesianProduct vecAdd future_enemies_pos
        possible_pos =
            unionList [ close_heroes, List.map .pos model.board.obstacles, future_enemies_pos ]
                |> listDifference model.board.map
    in
    if future_enemies_pos == [] then
        unionList [ close_heroes, List.map .pos model.board.obstacles ]
            |> listDifference model.board.map

    else
        possible_pos


{-| This function will generate a crate at random position and random item type.
-}
randomCrate : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
randomCrate msg ( model, cmd ) =
    case msg of
        EndTurn ->
            case model.board.turn of
                PlayerTurn ->
                    if Tuple.first model.board.mapRotating then
                        ( model, cmd )

                    else if possibleCratePosition model /= [] then
                        ( { model | board = turnTurret model.board }, Cmd.batch [ cmd, Random.generate SpawnCrate (generateCrate model) ] )

                    else
                        ( { model | board = turnTurret model.board }, cmd )

                _ ->
                    ( model, cmd )

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
            Random.uniform HealthPotion [ EnergyPotion, Gold 10 ]
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
    if (model.board.obstacles |> List.filter (\x -> x.obstacleType == MysteryBox) |> List.length) < 5 then
        unionList [ close_heroes, close_enemies, List.map .pos model.board.obstacles, List.map .pos model.board.item ]
            |> listDifference model.board.map

    else
        []


turnTurret : Board -> Board
turnTurret board =
    { board
        | turn = TurretTurn
        , enemies = List.sortBy .indexOnBoard board.enemies
        , heroes = List.map deselectHeroes board.heroes
        , timeTurn = 0
    }


deselectHeroes : Hero -> Hero
deselectHeroes hero =
    { hero | selected = False }
