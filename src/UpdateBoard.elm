module UpdateBoard exposing (..)

import Action exposing (index2Hero, pos2Item, selectedHero, unselectedHero)
import Board exposing (Board)
import Data exposing (..)
import EnemyAction exposing (actionEnemy)
import HeroAttack exposing (checkAttack)
import Message exposing (Msg(..))


updateBoard : Msg -> Board -> Board
updateBoard msg board =
    case msg of
        Key dir False ->
            case board.turn of
                EnemyTurn ->
                    board

                _ ->
                    moveHero board dir

        --        Select class False ->
        --            case board.turn of
        --                HeroTurn ->
        --                    selectHero board class
        --
        --                EnemyTurn ->
        --                    board
        Tick elapsed ->
            let
                nboard =
                    case board.turn of
                        PlayerTurn ->
                            board

                        AttackInProgress ->
                            { board | time = board.time + elapsed / 1000 }

                        EnemyTurn ->
                            { board | time = board.time + elapsed / 1000 }

                        MovingInProgress ->
                            { board | time = board.time + elapsed / 1000 }
            in
            if nboard.time > 0.5 && board.turn == EnemyTurn then
                { nboard | time = 0 }
                    |> actionEnemy
                    |> checkTurn

            else if nboard.time > 1 && board.turn == AttackInProgress || nboard.time > 1 && board.turn == MovingInProgress then
                { nboard | heroes = List.map returnHeroToWaiting board.heroes, enemies = List.map returnEnemyToWaiting board.enemies, turn = PlayerTurn, time = 0 }

            else
                nboard

        Attack pos critical ->
            checkAttack board pos critical

        SpawnEnemy ( list_class, list_pos ) ->
            spawnEnemies list_class list_pos board

        SpawnCrate ( pos, itype ) ->
            spawnCrate pos itype board

        Kill False ->
            { board | enemies = [] }

        _ ->
            board


returnHeroToWaiting : Hero -> Hero
returnHeroToWaiting hero =
    case hero.state of
        Attacked _ ->
            { hero | state = Waiting }

        Attacking ->
            { hero | state = Waiting }

        Moving ->
            { hero | state = Waiting }

        _ ->
            hero


returnEnemyToWaiting : Enemy -> Enemy
returnEnemyToWaiting enemy =
    case enemy.state of
        Attacked _ ->
            { enemy | state = Waiting }

        Attacking ->
            { enemy | state = Waiting }

        Moving ->
            { enemy | state = Waiting }

        _ ->
            enemy


turnEnemy : Board -> Board
turnEnemy board =
    { board
        | turn = EnemyTurn
        , enemies = List.map resetSteps board.enemies
        , heroes = List.map deselectHeroes (List.map resetEnergy board.heroes)
        , time = 0
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


checkTurn : Board -> Board
checkTurn board =
    if List.all (\enemy -> enemy.done) board.enemies then
        { board | turn = PlayerTurn }

    else
        board


moveHero : Board -> Dir -> Board
moveHero board dir =
    let
        dr =
            case dir of
                W ->
                    ( -1, 0 )

                E ->
                    ( 0, -1 )

                D ->
                    ( 1, -1 )

                X ->
                    ( 1, 0 )

                Z ->
                    ( 0, 1 )

                A ->
                    ( -1, 1 )

                _ ->
                    ( 0, 0 )

        ( nboard, ind ) =
            case selectedHero board.heroes of
                Nothing ->
                    ( board, Nothing )

                Just hero ->
                    let
                        currEnergy =
                            hero.energy

                        newPos =
                            vecAdd hero.pos dr
                    in
                    if legalHeroMove board hero dr && hero.energy > 1 then
                        ( { board | heroes = { hero | pos = newPos, energy = currEnergy - 2, state = Moving } :: unselectedHero board.heroes, turn = MovingInProgress }
                        , Just hero.indexOnBoard
                        )

                    else
                        ( board, Just hero.indexOnBoard )
    in
    case ind of
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
            { board
                | heroes = { hero | health = hero.health + 10 } :: otherHeroes
                , item = otherItems
            }

        Gold n ->
            { board
                | item = otherItems
                , coins = board.coins + n
            }

        EnergyPotion ->
            { board
                | heroes = { hero | energy = hero.energy + 2 } :: otherHeroes
                , item = otherItems
            }

        Buff ->
            board

        NoItem ->
            board



-- To be rewritten later


legalHeroMove : Board -> Hero -> Pos -> Bool
legalHeroMove board hero dr =
    List.member (vecAdd hero.pos dr) board.map && not (List.member (vecAdd hero.pos dr) (List.map .pos board.obstacles ++ List.map .pos board.heroes ++ List.map .pos board.enemies))



{-
   legalEnemyMove : Board -> List Hero -> Enemy -> Bool
   legalEnemyMove board hero_list enemy =
       List.member enemy.pos board.map && not (List.member enemy.pos (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemies))
-}


selectHero : Board -> Int -> Board
selectHero board index =
    let
        ( wantedHero, unwantedHero ) =
            List.partition (\hero -> hero.indexOnBoard == index) board.heroes

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
