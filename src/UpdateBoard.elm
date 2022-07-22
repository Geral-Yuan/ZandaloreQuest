module UpdateBoard exposing (..)

import Action exposing (index2Hero, pos2Item, selectedHero, unMoveable, unselectedHero, updateEnemyAttackable)
import Board exposing (Board)
import Data exposing (..)
import EnemyAction exposing (actionEnemy, checkEnemyDone)
import HeroAttack exposing (checkAttack)
import Message exposing (Msg(..))


updateBoard : Msg -> Board -> Board
updateBoard msg board =
    case msg of
        Tick elapsed ->
            let
                nBoard =
                    case board.turn of
                        PlayerTurn ->
                            case board.boardState of
                                NoActions ->
                                    board

                                _ ->
                                    { board | timeBoardState = board.timeBoardState + elapsed / 1000 }

                        EnemyTurn ->
                            case board.boardState of
                                NoActions ->
                                    { board | timeTurn = board.timeTurn + elapsed / 1000 }

                                _ ->
                                    { board | timeBoardState = board.timeBoardState + elapsed / 1000 }

                updatedBoard =
                    if board.boardState /= NoActions && nBoard.timeBoardState > 1.0 then
                        { nBoard | heroes = List.map returnHeroToWaiting nBoard.heroes, enemies = nBoard.enemies |> List.map returnEnemyToWaiting |> List.map checkEnemyDone, boardState = NoActions, timeBoardState = 0 }

                    else if board.boardState == NoActions && nBoard.timeTurn > 1.0 then
                        { nBoard | timeTurn = 0, enemies = nBoard.enemies |> List.map checkEnemyDone }
                            |> actionEnemy

                    else if board.timeTurn > 0.5 then
                        { nBoard | enemies = nBoard.enemies |> List.map checkEnemyDone }

                    else
                        nBoard
            in
            updatedBoard |> checkCurrentEnemy |> updateEnemyAttackable |> checkTurn

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
                    selectHero board hero

                EnemyTurn ->
                    board

        Move pos ->
            moveHero board pos

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


turnEnemy : Board -> Board
turnEnemy board =
    { board
        | turn = EnemyTurn
        , enemies = List.sortBy .indexOnBoard (List.map resetSteps board.enemies)
        , heroes = List.map deselectHeroes (List.map resetEnergy board.heroes)
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
    if List.all (\enemy -> enemy.done) board.enemies then
        { board | turn = PlayerTurn }

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
