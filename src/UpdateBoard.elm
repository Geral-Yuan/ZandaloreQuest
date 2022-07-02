module UpdateBoard exposing (..)

import Board exposing (Board)
import Data exposing (..)
import HeroAttack exposing (checkAttack, selectedHero, unselectedHero)
import Message exposing (Msg(..))
import ShortestPath exposing (leastPath)


updateBoard : Msg -> Board -> Board
updateBoard msg board =
    case msg of
        Key dir False ->
            case board.turn of
                HeroTurn ->
                    moveHero board dir

                EnemyTurn ->
                    board

        Select class False ->
            case board.turn of
                HeroTurn ->
                    selectHero board class

                EnemyTurn ->
                    board

        EndTurn ->
            turnEnemy board

        Tick elapsed ->
            case board.turn of
                HeroTurn ->
                    board

                EnemyTurn ->
                    { board | time = board.time + elapsed / 1000 }
                        |> moveEnemy
                        |> checkTurn

        Attack pos critical ->
            checkAttack board pos critical

        _ ->
            board


turnEnemy : Board -> Board
turnEnemy board =
    { board
        | turn = EnemyTurn
        , enemies = List.map (\enemy -> { enemy | done = False, steps = 2 }) board.enemies
        , heroes = List.map deselectHeroes (List.map resetEnergy board.heroes)
    }


resetEnergy : Hero -> Hero
resetEnergy hero =
    case hero.class of
        Warrior ->
            { hero | energy = 5 }

        Archer ->
            { hero | energy = 5 }

        Mage ->
            { hero | energy = 5 }

        Assassin ->
            { hero | energy = 6 }

        Healer ->
            { hero | energy = 6 }


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
    in
    case selectedHero board.heroes of
        Nothing ->
            board

        Just hero ->
            if legalHeroMove board hero dr && hero.energy > 1 then
                let
                    currEnergy =
                        hero.energy
                in
                { board | heroes = { hero | pos = vecAdd hero.pos dr, energy = currEnergy - 2 } :: unselectedHero board.heroes }

            else
                board



-- To be rewritten later


moveEnemy : Board -> Board
moveEnemy board =
    let
        ( newEnemy, newboard ) =
            moveEnemyList board board.enemies
    in
    { newboard | enemies = newEnemy }


moveEnemyList : Board -> List Enemy -> ( List Enemy, Board )
moveEnemyList board enemy_list =
    case enemy_list of
        [] ->
            ( [], board )

        enemy :: restEnemy ->
            let
                ( movedEnemy, newboard ) =
                    --moveOneEnemy model enemy
                    moveSmartWarrior board enemy

                ( newrestEnemy, nnewboard ) =
                    moveEnemyList newboard restEnemy
            in
            ( movedEnemy :: newrestEnemy, nnewboard )


moveSmartWarrior : Board -> Enemy -> ( Enemy, Board )
moveSmartWarrior board enemy =
    let
        route =
            leastPath enemy board board.heroes
    in
    case route of
        [] ->
            if not enemy.done then
                ( { enemy | done = True }
                , { board
                    | time = 0
                    , heroes =
                        List.map (enermyWarriorAttack enemy.pos 5 0) board.heroes
                            |> List.filter (\x -> x.health > 0)
                  }
                )

            else
                ( enemy, board )

        first :: _ ->
            if board.time > 0.5 && not enemy.done then
                ( checkEnemyDone { enemy | steps = enemy.steps - 1, pos = first }, { board | time = 0 } )

            else
                ( enemy, board )


enermyWarriorAttack : Pos -> Int -> Int -> Hero -> Hero
enermyWarriorAttack my_enemy_pos damage critical hero =
    let
        newHealth =
            hero.health - damage - critical
    in
    if isWarriorAttackRange hero.pos my_enemy_pos then
        { hero | health = newHealth }

    else
        hero


checkEnemyDone : Enemy -> Enemy
checkEnemyDone enemy =
    if enemy.steps == 0 then
        { enemy | done = True }

    else
        enemy


legalHeroMove : Board -> Hero -> Pos -> Bool
legalHeroMove board hero dr =
    List.member (vecAdd hero.pos dr) board.map && not (List.member (vecAdd hero.pos dr) (board.barrier ++ List.map .pos board.heroes ++ List.map .pos board.enemies))



{-
   legalEnemyMove : Board -> List Hero -> Enemy -> Bool
   legalEnemyMove board hero_list enemy =
       List.member enemy.pos board.map && not (List.member enemy.pos (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemies))
-}


selectHero : Board -> Class -> Board
selectHero board class =
    let
        ( wantedHero, unwantedHero ) =
            List.partition (\hero -> hero.class == class) board.heroes

        newwantedHero =
            List.map (\hero -> { hero | selected = True }) wantedHero

        newunwantedHero =
            List.map (\hero -> { hero | selected = False }) unwantedHero
    in
    { board | heroes = newwantedHero ++ newunwantedHero }



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
