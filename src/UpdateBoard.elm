module UpdateBoard exposing (checkCurrentTurret, turnTurret, updateBoardAnimation, updateBoardOthers, updateTurretAttackable)

import Action exposing (attackedByHeroArcherRange, index2Hero, pos2Item, selectedHero, unMoveable, unselectedHero, updateEnemyAttackable)
import Board exposing (Board)
import Data exposing (..)
import EnemyAction exposing (actionEnemy, checkEnemyDone)
import HeroAttack exposing (checkAttack, heroTurretAttack)
import Message exposing (Msg(..))
import Svg.Attributes exposing (rotate)


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
            { board | enemies = [] }

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

        _ ->
            board


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
                |> rotate ( 5, 5 ) 3 True
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
                    if distance clickpos hero.pos == 1 && not (List.member clickpos (unMoveable board)) && hero.energy >= 2 then
                        ( { board | heroes = { hero | pos = clickpos, energy = hero.energy - 2, state = Moving } :: unselectedHero board.heroes, boardState = HeroMoving }
                        , Just hero.indexOnBoard
                        )

                    else
                        ( board, Nothing )
    in
    case index of
        Nothing ->
            board

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
                    min (hero.health + 10) hero.maxHealth

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
                | heroes = { hero | energy = hero.energy + 2, state = TakingEnergy } :: otherHeroes
                , item = otherItems
                , boardState = HeroEnergy
            }

        Buff ->
            board

        NoItem ->
            board



-- To be rewritten later
{-
   legalEnemyMove : Board -> List Hero -> Enemy -> Bool
   legalEnemyMove board hero_list enemy =
       List.member enemy.pos board.map && not (List.member enemy.pos (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemies))
-}


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



{-
   -- the stats of each enemies can be changed later


   mapClassEnemy : Class -> Pos -> Int -> Enemy
   mapClassEnemy class pos idx =
       case class of
           Warrior ->
               Enemy class pos 100 10 0 False idx

           Archer ->
               Enemy class pos 100 10 0 False idx

           Assassin ->
               Enemy class pos 100 10 0 False idx

           Mage ->
               Enemy class pos 100 10 0 False idx

           Healer ->
               Enemy class pos 100 10 0 False idx
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



-- wait for more scene
{-
   checkEnd : Model -> Model
   checkEnd model =
       if List.length model.heroes == 0 then
           model

       else if List.length model.board.enemies == 0 then
           model

       else
           model
-}


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
