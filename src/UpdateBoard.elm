module UpdateBoard exposing (updateBeaten, updateBoardGame)

import Action exposing (attackedByHeroArcherRange, index2Hero, pos2Item, selectedHero, unMoveable, unselectedHero, updateAttackable, updateEnemyAttackable, updateTarget)
import Bag exposing (addCoin)
import BoardMap exposing (rotateStuff)
import Data exposing (sampleEnemy)
import EnemyAction exposing (actionEnemy, checkEnemyDone)
import HeroAttack exposing (checkAttack, generateDamage, heroTurretAttack)
import ListOperation exposing (cartesianProduct, listDifference, unionList)
import Message exposing (Msg(..))
import NPC exposing (npcMap)
import Random exposing (Generator)
import Type exposing (..)
import VectorOperation exposing (distance, neighbour, subneighbour, vecAdd)
import ViewConst exposing (pixelHeight, pixelWidth)


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


turnTurret : Board -> Board
turnTurret board =
    { board
        | turn = TurretTurn
        , enemies = List.sortBy .indexOnBoard board.enemies
        , heroes = List.map deselectHeroes board.heroes
        , timeTurn = 0
    }


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
        Warrior ->
            { hero | energy = 5 }

        Archer ->
            { hero | energy = 5 }

        Mage ->
            { hero | energy = 3 }

        Assassin ->
            { hero | energy = 6 }

        Healer ->
            { hero | energy = 5 }

        Engineer ->
            { hero | energy = 5 }

        _ ->
            { hero | energy = 0 }


deselectHeroes : Hero -> Hero
deselectHeroes hero =
    { hero | selected = False }


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


updateMap : Int -> Board -> Board
updateMap level board =
    case level of
        5 ->
            { board | mapRotating = ( True, 0 ) }
                |> rotate ( 5, 5 ) 4 False
                |> rotate ( 5, 5 ) 2 True

        6 ->
            { board | mapRotating = ( True, 0 ) }
                |> rotate ( 5, 5 ) 4 True
                |> rotate ( 5, 5 ) 3 True
                |> rotate ( 5, 5 ) 2 True
                |> rotate ( 5, 5 ) 1 False
                |> rotate ( 2, 5 ) 1 True
                |> rotate ( 2, 8 ) 1 False
                |> rotate ( 5, 8 ) 1 True
                |> rotate ( 8, 5 ) 1 False
                |> rotate ( 8, 2 ) 1 True
                |> rotate ( 5, 2 ) 1 False

        _ ->
            board


rotate : Pos -> Int -> Bool -> Board -> Board
rotate center dis clockwise board =
    let
        ( targetHeroes, restHeroes ) =
            List.partition (\hero -> distance center hero.pos == dis) board.heroes

        ( targetEnemies, restEnemies ) =
            List.partition (\hero -> distance center hero.pos == dis) board.enemies

        ( targetObstacles, restObstacles ) =
            List.partition (\hero -> distance center hero.pos == dis) board.obstacles

        ( targetItems, restItems ) =
            List.partition (\hero -> distance center hero.pos == dis) board.item
    in
    { board
        | heroes = List.map (rotateStuff clockwise center) targetHeroes ++ restHeroes
        , enemies = List.map (rotateStuff clockwise center) targetEnemies ++ restEnemies
        , obstacles = List.map (rotateStuff clockwise center) targetObstacles ++ restObstacles
        , item = List.map (rotateStuff clockwise center) targetItems ++ restItems
    }


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


spawnCrate : Pos -> ItemType -> Board -> Board
spawnCrate pos itype board =
    let
        crate =
            Obstacle MysteryBox pos itype

        nobstacles =
            crate :: board.obstacles
    in
    { board | obstacles = nobstacles }


actionTurret : Board -> Board
actionTurret board =
    let
        undoneTurret =
            List.filter (\turret -> (turret.class == Turret) && (turret.energy == 0)) board.heroes

        selectboard =
            case undoneTurret of
                [] ->
                    board

                turret :: _ ->
                    let
                        attackedenemy =
                            heroTurretAttack turret board
                    in
                    if attackedenemy /= board.enemies then
                        { board
                            | enemies = attackedenemy |> List.filter (\x -> x.health > 0)
                            , heroes = { turret | energy = -3, state = Attacking } :: listDifference board.heroes [ turret ]
                            , boardState = HeroAttack
                            , attackable = attackedByHeroArcherRange board turret.pos
                        }

                    else
                        { board
                            | heroes = { turret | energy = -3, state = Waiting } :: listDifference board.heroes [ turret ]
                            , attackable = attackedByHeroArcherRange board turret.pos
                        }
    in
    selectboard


checkCurrentTurret : Board -> Board
checkCurrentTurret board =
    let
        undoneTurret =
            List.filter (\turret -> (turret.class == Turret) && (turret.energy /= -6)) board.heroes
    in
    case undoneTurret of
        [] ->
            { board | cntTurret = 0 }

        enemy :: _ ->
            { board | cntTurret = enemy.indexOnBoard }


updateTurretAttackable : Board -> Board
updateTurretAttackable board =
    let
        maybeTurret =
            List.head (List.filter (\x -> x.indexOnBoard == board.cntTurret) board.heroes)
    in
    if board.turn == TurretTurn then
        case maybeTurret of
            Nothing ->
                { board | attackable = [] }

            Just turret ->
                let
                    realattackRange =
                        attackedByHeroArcherRange board turret.pos
                in
                { board | attackable = realattackRange }

    else
        board


checkTurretDone : Hero -> Hero
checkTurretDone turret =
    if turret.energy == -3 && turret.state == Waiting && turret.class == Turret then
        { turret | energy = -6 }

    else
        turret


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


updateBeaten : List NPC -> List NPC
updateBeaten npclist =
    List.map (\npc -> { npc | beaten = True }) npclist
