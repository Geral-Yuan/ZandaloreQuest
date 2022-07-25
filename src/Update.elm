module Update exposing (update)

import Action exposing (updateAttackable, updateMoveable, updateTarget)
import Bag exposing (addCoin)
import Board exposing (initBoard)
import Data exposing (..)
import HeroAttack exposing (generateDamage)
import Message exposing (Msg(..))
import Model exposing (Model)
import NPC exposing (npcDarkKnight1, npcDarkKnight2)
import Random exposing (Generator)
import RpgCharacter exposing (moveCharacter)
import Svg.Attributes exposing (mode)
import UpdateBoard exposing (checkCurrentTurret, turnTurret, updateBoardAnimation, updateBoardOthers, updateTurretAttackable)
import UpdateScene exposing (..)
import UpdateShop exposing (updateShop)
import ViewNPCTask exposing (checkTalkRange)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( nmodel, ncmd ) =
            case model.mode of
                Tutorial k ->
                    updateTutorial msg k model

                BoardGame ->
                    updateBoardGame msg model

                Summary ->
                    updateSummary msg model

                Logo ->
                    ( updateScene msg model, Cmd.none )

                Dialog task ->
                    updateDialog msg task model

                HeroChoose ->
                    ( model
                        |> checkChooseClick msg
                        |> checkConfirm msg
                    , Cmd.none
                    )

                BuyingItems ->
                    updateShop msg model

                UpgradePage ->
                    updateShop msg model

                _ ->
                    updateRPG msg model
                        |> updateCharacter msg
    in
    ( nmodel
        |> resize msg
        |> getviewport msg
    , ncmd
    )


updateBoardGame : Msg -> Model -> ( Model, Cmd Msg )
updateBoardGame msg model =
    case msg of
        ViewTutorial ->
            ( { model | mode = Tutorial 1 }, Cmd.none )

        _ ->
            { model | board = model.board |> updateBoardAnimation msg |> updateBoardOthers msg |> updateAttackable |> updateMoveable |> updateTarget |> checkCurrentTurret |> updateTurretAttackable }
                |> checkMouseMove msg
                |> checkHit msg
                |> randomCrate msg
                |> randomEnemies
                |> checkEnd


updateSummary : Msg -> Model -> ( Model, Cmd Msg )
updateSummary msg model =
    case msg of
        Click _ _ ->
            ( { model | mode = model.previousMode, level = model.level + 1 }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateDialog : Msg -> Task -> Model -> ( Model, Cmd Msg )
updateDialog msg task model =
    case task of
        MeetElder ->
            if msg == Enter False then
                ( { model | mode = HeroChoose }, Cmd.none )

            else
                ( model, Cmd.none )

        FinishTutorial ->
            if msg == Enter False then
                ( { model | mode = Castle }, Cmd.none )

            else
                ( model, Cmd.none )

        --To be changed when it's other tasks
        _ ->
            if msg == Enter False then
                ( { model | mode = HeroChoose }, Cmd.none )

            else
                ( model, Cmd.none )


followTutorial : Msg -> Int -> Bool
followTutorial msg k =
    case k of
        2 ->
            case msg of
                Select hero ->
                    hero.class == Warrior

                _ ->
                    False

        3 ->
            msg == Move ( 3, 7 )

        4 ->
            msg == Move ( 4, 6 )

        5 ->
            case msg of
                Select hero ->
                    hero.class == Archer

                _ ->
                    False

        6 ->
            msg == Hit ( 5, 4 )

        7 ->
            msg == EndTurn

        8 ->
            case msg of
                Select hero ->
                    hero.class == Warrior

                _ ->
                    False

        9 ->
            msg == Move ( 4, 5 )

        10 ->
            msg == Move ( 5, 4 )

        11 ->
            case msg of
                Select hero ->
                    hero.class == Archer

                _ ->
                    False

        12 ->
            msg == Hit ( 7, 2 )

        13 ->
            msg == Enter False

        _ ->
            False


updateTutorial : Msg -> Int -> Model -> ( Model, Cmd Msg )
updateTutorial msg k model =
    case k of
        1 ->
            if msg == Enter False then
                ( { model | mode = Tutorial 2 }, Cmd.none )

            else
                ( model, Cmd.none )

        14 ->
            ( { model | mode = BoardGame }, Cmd.none )

        _ ->
            case msg of
                Tick _ ->
                    updateBoardGame msg model

                Attack _ _ ->
                    updateBoardGame msg model

                Kill False ->
                    ( { model
                        | mode = Dialog FinishTutorial
                        , level = model.level + 1
                        , cntTask = nextTask model.cntTask
                        , bag = addCoin model.bag 50
                        , npclist = (model.npclist |> updateBeaten) ++ nextNPC model.cntTask
                      }
                    , Cmd.none
                    )

                _ ->
                    if followTutorial msg k && model.board.boardState == NoActions then
                        { model | mode = Tutorial (k + 1) } |> updateBoardGame msg

                    else
                        ( model, Cmd.none )


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

                chosenidx =
                    findChosenHero clickpos

                index =
                    if List.map Tuple.second model.indexedheroes |> List.member chosenidx then
                        chosenidx

                    else
                        0
            in
            if index > 0 && index <= 6 then
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


checkConfirm : Msg -> Model -> Model
checkConfirm msg model =
    let
        level =
            model.level
    in
    case msg of
        Confirm ->
            if List.length model.chosenHero == 3 && model.level == 0 then
                { model | mode = Tutorial 1, board = initBoard (confirmHeroes model) level, chosenHero = [] }

            else if List.length model.chosenHero == 3 then
                { model | mode = BoardGame, board = initBoard (confirmHeroes model) level, chosenHero = [] }

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
            if isReachable model.mode newCharacter.pos model.npclist then
                ( { model | character = newCharacter }, cmd )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


isReachable : GameMode -> ( Float, Float ) -> List NPC -> Bool
isReachable mode ( x, y ) npclist =
    case mode of
        Castle ->
            ((x > 310 && x < 1692 && y > 782 && y < 812)
                || (x > 572 && x < 1427 && y <= 782 && y > 407)
                || (x > 732 && x < 1272 && y <= 407 && y > 382)
                || (y <= 407 && y > 197 && (x > 572 && x < 667 || x > 1332 && x < 1427))
                || (y <= 197 && y > 167 && x > 322 && x < 1692)
                || (y <= 602 && y > 197 && (x > 322 && x < 497 || x > 1492 && x < 1692))
                || (y <= 167 && y > 32 && (x > 322 && x < 447 || x > 1557 && x < 1692))
            )
                && not (List.foldr (||) False (List.map (npcCollisionRange ( x, y )) (npclist |> List.filter (\npc -> npc.scene == CastleScene))))

        Shop ->
            (y >= 782 && y < 902 && x > 682 && x < 902)
                || (x > 392 && x < 1217 && y >= 582 && y < 782)
                || (x > 392 && x < 462 && y >= 410 && y < 582)

        Dungeon ->
            y > 241 && y < 974 && x > 502 && x < 1542

        Dungeon2 ->
            y > 241 && y < 974 && x > 502 && x < 1542

        _ ->
            True


npcCollisionRange : ( Float, Float ) -> NPC -> Bool
npcCollisionRange ( x, y ) npc =
    let
        ( nx, ny ) =
            npc.position

        ( nw, nh ) =
            npc.size
    in
    x > nx - nw + 20 && x < nx + nw - 20 && y > ny - nh && y < ny + nh - 30



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
    ( { currHero | health = currHealth + 5, maxHealth = currHealth + 5 }, index )


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

        bag =
            model.bag

        currHeroes =
            model.indexedheroes
    in
    case msg of
        Enter False ->
            case model.mode of
                Shop ->
                    ( model |> checkLeaveShop, Cmd.none )

                Castle ->
                    model |> checkLeaveCastle

                Dungeon ->
                    ( model |> checkLeaveDungeon, Cmd.none )

                Dungeon2 ->
                    ( model |> checkLeaveDungeon2, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Talk False ->
            if model.mode == Shop then
                if x > 600 && x < 1000 && y > 650 then
                    ( { model | mode = BuyingItems }, Cmd.none )

                else
                    ( model, Cmd.none )

            else
                ( model |> checkTalkRange, Cmd.none )

        Key Left on ->
            ( { model | character = { character | moveLeft = on, moveRight = character.moveRight && not on } }, Cmd.none )

        Key Right on ->
            ( { model | character = { character | moveRight = on, moveLeft = character.moveLeft && not on } }, Cmd.none )

        Key Up on ->
            ( { model | character = { character | moveUp = on, moveDown = character.moveDown && not on } }, Cmd.none )

        Key Down on ->
            ( { model | character = { character | moveDown = on, moveUp = character.moveUp && not on } }, Cmd.none )

        UpgradeHealth ->
            if model.bag.coins > 49 then
                ( { model | bag = { bag | coins = currCoins - 50 }, indexedheroes = List.map updateHealth currHeroes }, Cmd.none )

            else
                ( model, Cmd.none )

        UpgradeDamage ->
            if model.bag.coins > 49 then
                ( { model | bag = { bag | coins = currCoins - 50 }, indexedheroes = List.map updateDamage currHeroes }, Cmd.none )

            else
                ( model, Cmd.none )

        ExitShop ->
            ( { model | mode = Shop }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateScene : Msg -> Model -> Model
updateScene msg model =
    case msg of
        Tick elapsed ->
            { model | time = model.time + elapsed / 1000 }
                |> checkLogoEnd

        Enter False ->
            { model | mode = Castle }

        _ ->
            model


checkLogoEnd : Model -> Model
checkLogoEnd model =
    if (model.time > 6.1) && (model.mode == Logo) then
        { model | mode = Castle }

    else
        model


resize : Msg -> Model -> Model
resize msg model =
    case msg of
        Resize width height ->
            { model | size = ( toFloat width, toFloat height ) }

        _ ->
            model


checkHit : Msg -> Model -> ( Model, Cmd Msg )
checkHit msg model =
    case msg of
        Hit pos ->
            case model.board.turn of
                PlayerTurn ->
                    if model.board.boardState == NoActions then
                        ( model, generateDamage pos )

                    else
                        ( model, Cmd.none )

                TurretTurn ->
                    ( model, Cmd.none )

                EnemyTurn ->
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
            case model.board.turn of
                PlayerTurn ->
                    if possibleCratePosition model /= [] then
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

        finalboard =
            { myboard
                | heroes = List.filter (\x -> x.health > 0) model.board.heroes
                , enemies = List.filter (\x -> x.health > 0) model.board.enemies
            }

        wincoins =
            myboard.coins + 50

        losecoins =
            myboard.coins

        nmodel =
            case model.mode of
                BoardGame ->
                    if
                        List.isEmpty finalboard.enemies
                            && (finalboard.spawn == 0)
                            && (model.level == 0)
                            && List.all (\hero -> hero.state == Waiting) model.board.heroes
                    then
                        { model
                            | mode = Dialog FinishTutorial
                            , level = model.level + 1
                            , cntTask = nextTask model.cntTask
                            , bag = addCoin model.bag wincoins
                            , npclist = (model.npclist |> updateBeaten) ++ nextNPC model.cntTask
                            , unlockShop = True
                        }

                    else if
                        List.isEmpty finalboard.enemies
                            && (finalboard.spawn == 0)
                            && (model.level /= 0)
                            && List.all (\hero -> hero.state == Waiting) model.board.heroes
                    then
                        { model
                            | mode = Summary
                            , cntTask = nextTask model.cntTask
                            , bag = addCoin model.bag wincoins
                            , npclist = (model.npclist |> updateBeaten) ++ nextNPC model.cntTask
                        }

                    else if
                        List.isEmpty finalboard.heroes
                            && List.all (\enemy -> enemy.state == Waiting) model.board.enemies
                    then
                        { model
                            | mode = model.previousMode
                            , bag = addCoin model.bag losecoins
                        }

                    else
                        model

                _ ->
                    model
    in
    ( nmodel, cmd )



-- To be modified


nextTask : Task -> Task
nextTask task =
    case task of
        MeetElder ->
            GoToShop

        GoToShop ->
            Level 1

        Level k ->
            Level (k + 1)

        _ ->
            BeatBoss


nextNPC : Task -> List NPC
nextNPC task =
    case task of
        MeetElder ->
            [ npcDarkKnight1 ]

        Level 1 ->
            [ npcDarkKnight2 ]

        _ ->
            []


updateBeaten : List NPC -> List NPC
updateBeaten npclist =
    List.map (\npc -> { npc | beaten = True }) npclist
