module UpdateBoard exposing (..)

import Action exposing (selectedHero, unselectedHero, pos2Item)
import Board exposing (Board)
import Data exposing (..)
import EnemyAction exposing (actionEnemy)
import HeroAttack exposing (checkAttack)
import Message exposing (Msg(..))
import Action exposing (pos2Hero)


updateBoard : Msg -> Board -> Board
updateBoard msg board =
    case msg of
        Key dir False ->
            case board.turn of
                HeroTurn ->
                    moveHero board dir

                EnemyTurn ->
                    board

        --        Select class False ->
        --            case board.turn of
        --                HeroTurn ->
        --                    selectHero board class
        --
        --                EnemyTurn ->
        --                    board
        EndTurn ->
            turnEnemy board

        Tick elapsed ->
            let
                nboard =
                    case board.turn of
                        HeroTurn ->
                            board

                        EnemyTurn ->
                            { board | time = board.time + elapsed / 1000 }
            in
            if nboard.time > 0.5 then
                { nboard | time = 0 }
                    |> actionEnemy
                    |> checkTurn

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


deselectHeroes : Hero -> Hero
deselectHeroes hero =
    { hero | selected = False }


checkTurn : Board -> Board
checkTurn board =
    if List.all (\enemy -> enemy.done) board.enemies then
        { board | turn = HeroTurn }

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

        (doneboard, donepos) = case selectedHero board.heroes of
                        Nothing ->
                            (board, Nothing)

                        Just hero ->
                            let
                                currEnergy =
                                    hero.energy

                                newPos =
                                    vecAdd hero.pos dr

                            in
                            if legalHeroMove board hero dr && hero.energy > 1 then
                                ({ board | heroes = { hero | pos = newPos, energy = currEnergy - 2 } :: unselectedHero board.heroes }
                                , Just newPos)
                            else
                                (board, Just hero.pos)
    in
    case donepos of
        Nothing ->
            board

        Just pos ->
            checkHeroItem (pos2Hero doneboard.heroes pos) doneboard
    


checkHeroItem : Hero -> Board -> Board
checkHeroItem hero board =
    let
        otherHeroes = listDifference board.heroes [hero]
        chosenItem = pos2Item board.item hero.pos
        otherItems = listDifference board.item [chosenItem]
    in
    case chosenItem.itemType of
        HealthPotion ->
            { board | heroes = { hero | health = hero.health + 10 } :: otherHeroes
                    , item = otherItems }

        Gold n ->
            { board | item = otherItems
                    , coins = board.coins + n }

        EnergyPotion ->
            board

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
                    |> List.map3 mapClassEnemy list_class list_pos
        in
        { board | enemies = n_enemies, spawn = board.spawn - 1 }

    else
        board



-- the stats of each enemies can be changed later


mapClassEnemy : Class -> Pos -> Int -> Enemy
mapClassEnemy class pos idx =
    case class of
        Warrior ->
            Enemy class pos 100 10 5 0 False idx

        Archer ->
            Enemy class pos 100 10 5 0 False idx

        Assassin ->
            Enemy class pos 100 10 5 0 False idx

        Mage ->
            Enemy class pos 100 10 5 0 False idx

        Healer ->
            Enemy class pos 100 10 5 0 False idx


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
