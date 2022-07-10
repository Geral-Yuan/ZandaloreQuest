module Update exposing (update)

import Action exposing (updateAttackable, updateMoveable, updateTarget)
import Bag exposing (addCoin)
import Board exposing (initBoard)
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
                Tutorial 3 ->
                    case msg of
                        Enter False ->
                            ( { model | mode = model.previousMode }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Tutorial 1 ->
                    case msg of
                        Enter False ->
                            ( { model | mode = Tutorial 2 }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Tutorial 2 ->
                    case msg of
                        Enter False ->
                            ( { model | mode = Tutorial 3 }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                BoardGame _ ->
                    case msg of
                        ViewTutorial ->
                            ( { model | previousMode = model.mode, mode = Tutorial 1 }, Cmd.none )

                        _ ->
                            { model | board = updateBoard msg model.board |> updateAttackable |> updateMoveable |> updateTarget }
                                |> checkMouseMove msg
                                |> checkSelectedClick msg
                                |> checkAttackClick msg
                                |> randomCrate msg
                                |> randomEnemies
                                |> checkEnd

                -- Logo ->
                --     ( updateScene msg model, Cmd.none )
                HeroChoose k ->
                    ( model
                        |> checkChooseClick msg
                        |> checkConfirm msg k
                    , Cmd.none
                    )

                _ ->
                    updateRPG msg model
    in
    ( nmodel
        |> resize msg
        |> getviewport msg
    , ncmd
    )


checkChooseClick : Msg -> Model -> Model
checkChooseClick msg model =
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

                index =
                    findChosenHero clickpos
            in
            if index > 0 && index <= List.length model.indexedheroes then
                if List.member index model.chosenHero then
                    { model | chosenHero = List.filter (\heroindex -> heroindex /= index) model.chosenHero }

                else if List.length model.chosenHero < 3 then
                    { model | chosenHero = index :: model.chosenHero }

                else
                    model

            else
                model

        _ ->
            model


checkConfirm : Msg -> Int -> Model -> Model
checkConfirm msg k model =
    case msg of
        Confirm ->
            if List.length model.chosenHero == 3 then
                { model | mode = BoardGame k, board = initBoard (confirmHeroes model) k }

            else
                model

        _ ->
            model


confirmHeroes : Model -> List Hero
confirmHeroes model =
    let
        chosenIndexedHeroes =
            List.filter (\( _, y ) -> List.member y model.chosenHero) model.indexedheroes

        sortedHeroes =
            Tuple.first (List.unzip (List.sortBy (\( _, y ) -> y) chosenIndexedHeroes))
    in
    initIndexOnBoard sortedHeroes


initIndexOnBoard : List Hero -> List Hero
initIndexOnBoard heroes =
    case List.reverse heroes of
        [] ->
            []

        lastHero :: rest ->
            initIndexOnBoard (List.reverse rest) ++ [ { lastHero | indexOnBoard = List.length heroes } ]


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
            (x > 250 && x < 1700 && y > 780 && y < 850) || (x > 580 && x < 1400 && y < 781 && y > 420)

        Shop ->
            x > 275 && x < 1740 && y > 590 && y < 801 || y > 800 && y < 905 && x > 650 && x < 900

        _ ->
            True



-- updateShop : Msg -> Model -> ( Model, Cmd Msg )
-- updateShop msg model =
--     let
--         currCoins =
--             model.bag.coins
--         newBag =
--             model.bag
--         currHeroes =
--             model.indexedheroes
--     in
--     case msg of
--         UpgradeHealth ->
--             if model.bag.coins > 49 then
--                 ( { model | bag = { newBag | coins = currCoins - 50 }, indexedheroes = List.map updateHealth currHeroes }, Cmd.none )
--             else
--                 ( model, Cmd.none )
--         UpgradeDamage ->
--             if model.bag.coins > 49 then
--                 ( { model | bag = { newBag | coins = currCoins - 50 }, indexedheroes = List.map updateDamage currHeroes }, Cmd.none )
--             else
--                 ( model, Cmd.none )
--         ExitShop ->
--             ( { model | mode = Shop }, Task.perform GetViewport getViewport )
--         _ ->
--             ( model, Cmd.none )


updateHealth : ( Hero, Int ) -> ( Hero, Int )
updateHealth hero =
    let
        currHero =
            Tuple.first hero

        index =
            Tuple.second hero

        currHealth =
            currHero.health
    in
    ( { currHero | health = currHealth + 5 }, index )


updateDamage : ( Hero, Int ) -> ( Hero, Int )
updateDamage hero =
    let
        currHero =
            Tuple.first hero

        index =
            Tuple.second hero

        currDamage =
            currHero.damage
    in
    ( { currHero | damage = currDamage + 2 }, index )


updateRPG : Msg -> Model -> ( Model, Cmd Msg )
updateRPG msg model =
    let
        character =
            model.character

        ( x, y ) =
            character.pos

        currCoins =
            model.bag.coins

        newBag =
            model.bag

        currHeroes =
            model.indexedheroes
    in
    case msg of
        Enter False ->
            case model.mode of
                Shop ->
                    if x > 710 && x < 900 && y > 850 then
                        ( { model | mode = Castle, character = { character | width = 65, height = 65, pos = ( 1630, 840 ) } }, Task.perform GetViewport getViewport )

                    else
                        ( model, Cmd.none )

                Castle ->
                    if x > 1600 && x < 1660 && y < 900 then
                        ( { model | mode = Shop, character = { character | width = 90, height = 90, pos = ( 800, 900 ) } }, Task.perform GetViewport getViewport )

                    else if x > 950 && x < 1050 && y < 450 then
                        ( { model | mode = HeroChoose 1 }, Task.perform GetViewport getViewport )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Talk False ->
            case model.mode of
                Shop ->
                    ( { model | mode = BuyingItems }, Cmd.none )

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

        UpgradeHealth ->
            if model.bag.coins > 49 then
                ( { model | bag = { newBag | coins = currCoins - 50 }, indexedheroes = List.map updateHealth currHeroes }, Cmd.none )

            else
                ( model, Cmd.none )

        UpgradeDamage ->
            if model.bag.coins > 49 then
                ( { model | bag = { newBag | coins = currCoins - 50 }, indexedheroes = List.map updateDamage currHeroes }, Cmd.none )

            else
                ( model, Cmd.none )

        ExitShop ->
            ( { model | mode = Shop }, Task.perform GetViewport getViewport )

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
            Random.uniform HealthPotion [ EnergyPotion, Gold 1 ]
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


checkEnd : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkEnd ( model, cmd ) =
    let
        myboard =
            model.board

        wincoins =
            myboard.coins + 100

        losecoins =
            myboard.coins

        nmodel =
            case model.mode of
                BoardGame _ ->
                    if List.isEmpty myboard.enemies then
                        { model
                            | mode = Castle
                            , bag = addCoin model.bag wincoins
                        }

                    else if List.isEmpty myboard.heroes then
                        { model
                            | mode = Castle
                            , bag = addCoin model.bag losecoins
                        }

                    else
                        model

                _ ->
                    model
    in
    ( nmodel, cmd )
