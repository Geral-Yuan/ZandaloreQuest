module UpdateBoard exposing (updateBeaten, updateBoardGame)

{-| This file fills functions related to updateing board game mode.


# Function

@docs updateBeaten, updateBoardGame

-}

import Action exposing (index2Hero, pos2Item, selectedHero, unMoveable, unselectedHero, updateAttackable, updateEnemyAttackable, updateTarget)
import Bag exposing (addCoin)
import EnemyAction exposing (actionEnemy, checkEnemyDone)
import HeroAttack exposing (checkAttack, generateDamage)
import ListOperation exposing (listDifference)
import Message exposing (Msg(..))
import NPC exposing (npcMap)
import Type exposing (Board, BoardState(..), Class(..), Enemy, FailToDo(..), GameMode(..), Hero, HeroState(..), ItemType(..), Model, NPC, Pos, Task(..), Turn(..))
import UpdateMap exposing (updateMap)
import UpdateSpawn exposing (randomCrate, randomEnemies, spawnCrate, spawnEnemies)
import UpdateTurret exposing (actionTurret, checkCurrentTurret, checkTurretDone, updateTurretAttackable)
import VectorOperation exposing (distance)
import ViewConst exposing (pixelHeight, pixelWidth)


{-| This function update board game mode
-}
updateBoardGame : Msg -> Model -> ( Model, Cmd Msg )
updateBoardGame msg model =
    { model | board = model.board |> updateBoardAnimation msg |> updateBoardOthers msg |> updateAttackable |> updateTarget |> checkCurrentTurret |> updateTurretAttackable }
        |> checkMouseMove msg
        |> checkHit msg
        |> randomCrate msg
        |> randomEnemies
        |> checkRotationDone
        |> checkEnd


updateBoardAnimation : Msg -> Board -> Board
updateBoardAnimation msg board =
    case msg of
        Tick elapsed ->
            let
                nBoard =
                    case board.turn of
                        PlayerTurn ->
                            case board.boardState of
                                NoActions ->
                                    let
                                        ( rotating, time ) =
                                            board.mapRotating
                                    in
                                    if rotating then
                                        { board | mapRotating = ( rotating, time + elapsed / 1000 ) }

                                    else
                                        board

                                _ ->
                                    { board | timeBoardState = board.timeBoardState + elapsed / 1000 }

                        _ ->
                            case board.boardState of
                                NoActions ->
                                    { board | timeTurn = board.timeTurn + elapsed / 1000 }

                                _ ->
                                    { board | timeBoardState = board.timeBoardState + elapsed / 1000 }

                updatedBoard =
                    if board.boardState /= NoActions && nBoard.timeBoardState > 1.0 then
                        { nBoard | heroes = List.map returnHeroToWaiting nBoard.heroes, enemies = nBoard.enemies |> List.map returnEnemyToWaiting |> List.map checkEnemyDone, boardState = NoActions, timeBoardState = 0 }

                    else if board.boardState == NoActions && nBoard.timeTurn > 1.0 && board.turn == EnemyTurn then
                        { nBoard | timeTurn = 0, enemies = nBoard.enemies |> List.map checkEnemyDone }
                            |> actionEnemy

                    else if board.boardState == NoActions && nBoard.timeTurn > 1.0 && board.turn == TurretTurn then
                        { nBoard | timeTurn = 0 }
                            |> actionTurret

                    else if board.timeTurn > 0.5 && board.turn == EnemyTurn then
                        { nBoard | enemies = nBoard.enemies |> List.map checkEnemyDone }

                    else if board.timeTurn > 0.5 && board.turn == TurretTurn then
                        { nBoard | heroes = nBoard.heroes |> List.map checkTurretDone }

                    else
                        nBoard
            in
            updatedBoard |> checkCurrentEnemy |> updateEnemyAttackable |> checkTurn

        _ ->
            board


updateBoardOthers : Msg -> Board -> Board
updateBoardOthers msg board =
    case msg of
        Attack pos critical ->
            checkAttack board pos critical

        SpawnEnemy ( list_class, list_pos ) ->
            spawnEnemies list_class list_pos board

        SpawnCrate ( pos, itype ) ->
            spawnCrate pos itype board

        Kill False ->
            { board | enemies = [], spawn = 0 }

        Select hero ->
            case board.turn of
                PlayerTurn ->
                    if board.boardState == NoActions && not (Tuple.first board.mapRotating) then
                        selectHero board hero

                    else
                        board

                TurretTurn ->
                    board

                EnemyTurn ->
                    board

        Move pos ->
            case board.turn of
                PlayerTurn ->
                    if board.boardState == NoActions then
                        moveHero board pos

                    else
                        board

                TurretTurn ->
                    board

                EnemyTurn ->
                    board

        ViewHint on ->
            { board | hintOn = on }

        _ ->
            board


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


checkRotationDone : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkRotationDone ( model, cmd ) =
    let
        board =
            model.board

        ( rotating, time ) =
            board.mapRotating
    in
    if rotating && time > 1 then
        ( { model | board = { board | mapRotating = ( False, 0 ) } }, cmd )

    else
        ( model, cmd )


checkEnd : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkEnd ( model, cmd ) =
    let
        myboard =
            model.board

        wincoins =
            myboard.coins + 50

        losecoins =
            myboard.coins

        win =
            List.isEmpty myboard.enemies && myboard.spawn == 0 && List.all (\hero -> hero.state == Waiting) model.board.heroes

        lose =
            List.isEmpty myboard.heroes && List.all (\enemy -> enemy.state == Waiting) model.board.enemies

        nmodel =
            case model.mode of
                BoardGame ->
                    if win && (model.level == 0) then
                        { model
                            | mode = Dialog FinishTutorial
                            , level = model.level + 1
                            , cntTask = nextTask model.cntTask
                            , bag = addCoin model.bag wincoins
                            , unlockShop = True
                            , npclist = model.npclist |> updateBeaten
                        }

                    else if win then
                        { model
                            | mode = Summary
                            , cntTask = nextTask model.cntTask
                            , bag = addCoin model.bag wincoins
                            , npclist = (model.npclist |> updateBeaten) ++ nextNPC model.cntTask
                            , unlockDungeon = model.unlockDungeon || (model.level == 2)
                            , unlockDungeon2 = model.unlockDungeon2 || (model.level == 4)
                        }

                    else if lose then
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


returnHeroToWaiting : Hero -> Hero
returnHeroToWaiting hero =
    case hero.state of
        Waiting ->
            hero

        _ ->
            { hero | state = Waiting }


returnEnemyToWaiting : Enemy -> Enemy
returnEnemyToWaiting enemy =
    case enemy.state of
        Waiting ->
            enemy

        _ ->
            { enemy | state = Waiting }


resetSteps : Enemy -> Enemy
resetSteps enemy =
    let
        nstep =
            case enemy.class of
                Mage ->
                    1

                Assassin ->
                    3

                Turret ->
                    4

                _ ->
                    2
    in
    { enemy | steps = nstep, done = False }


resetEnergy : Hero -> Hero
resetEnergy hero =
    case hero.class of
        Mage ->
            { hero | energy = 3 }

        Assassin ->
            { hero | energy = 6 }

        Turret ->
            { hero | energy = 0 }

        _ ->
            { hero | energy = 5 }


checkCurrentEnemy : Board -> Board
checkCurrentEnemy board =
    let
        ( _, undoneEnemy ) =
            List.partition .done board.enemies
    in
    case undoneEnemy of
        [] ->
            { board | cntEnemy = 0 }

        enemy :: _ ->
            { board | cntEnemy = enemy.indexOnBoard }


checkTurn : Board -> Board
checkTurn board =
    if List.all (\enemy -> enemy.done) board.enemies && board.turn == EnemyTurn then
        { board | turn = PlayerTurn, heroes = List.map resetEnergy board.heroes } |> updateMap board.level

    else if List.all (\turret -> turret.energy == -6) (List.filter (\x -> x.class == Turret) board.heroes) && board.turn == TurretTurn then
        { board | turn = EnemyTurn, enemies = List.map resetSteps board.enemies }

    else
        board


moveHero : Board -> Pos -> Board
moveHero board clickpos =
    let
        ( nboard, index ) =
            case selectedHero board.heroes of
                Nothing ->
                    ( board, Nothing )

                Just hero ->
                    if distance clickpos hero.pos == 1 && not (List.member clickpos (unMoveable board)) then
                        if hero.energy >= 2 then
                            ( { board | heroes = { hero | pos = clickpos, energy = hero.energy - 2, state = Moving } :: unselectedHero board.heroes, boardState = HeroMoving }
                            , Just hero.indexOnBoard
                            )

                        else
                            ( { board | popUpHint = ( LackEnergy, 0 ) }, Nothing )

                    else
                        ( board, Nothing )
    in
    case index of
        Nothing ->
            nboard

        Just n ->
            nboard
                |> checkHeroItem (index2Hero n nboard.heroes)


checkHeroItem : Hero -> Board -> Board
checkHeroItem hero board =
    let
        otherHeroes =
            listDifference board.heroes [ hero ]

        chosenItem =
            pos2Item board.item hero.pos

        otherItems =
            listDifference board.item [ chosenItem ]
    in
    case chosenItem.itemType of
        HealthPotion ->
            let
                nhealth =
                    min (hero.health + hero.maxHealth * 3 // 5) hero.maxHealth

                healthDif =
                    nhealth - hero.maxHealth
            in
            { board
                | heroes = { hero | health = nhealth, state = TakingHealth healthDif } :: otherHeroes
                , item = otherItems
                , boardState = HeroHealth
            }

        Gold n ->
            { board
                | item = otherItems
                , coins = board.coins + n
            }

        EnergyPotion ->
            { board
                | heroes = { hero | energy = fullEnergy hero.class, state = TakingEnergy } :: otherHeroes
                , item = otherItems
                , boardState = HeroEnergy
            }

        NoItem ->
            board


fullEnergy : Class -> Int
fullEnergy class =
    case class of
        Assassin ->
            6

        Mage ->
            3

        _ ->
            5


selectHero : Board -> Hero -> Board
selectHero board clickedhero =
    let
        ( wantedHero, unwantedHero ) =
            List.partition ((==) clickedhero) board.heroes

        newwantedHero =
            List.map (\hero -> { hero | selected = True }) wantedHero

        newunwantedHero =
            List.map (\hero -> { hero | selected = False }) unwantedHero
    in
    case clickedhero.class of
        Turret ->
            board

        _ ->
            { board | heroes = newwantedHero ++ newunwantedHero }


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
        Level 6 ->
            []

        Level k ->
            [ npcMap (k + 8) ]

        _ ->
            []


{-| This function will update the NPC list that has been beaten.
-}
updateBeaten : List NPC -> List NPC
updateBeaten npclist =
    List.map (\npc -> { npc | beaten = True }) npclist
