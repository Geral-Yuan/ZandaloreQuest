module UpdateBoard exposing (..)

import Action exposing (checkItemType, selectedHero, unselectedHero)
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
            if nboard.time > 1 then
                { nboard | time = 0 }
                    |> actionEnemy
                    |> checkTurn

            else
                nboard

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
        , time = 0
    }


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
    in
    case selectedHero board.heroes of
        Nothing ->
            board

        Just hero ->
            let
                currEnergy =
                    hero.energy

                newPos =
                    vecAdd hero.pos dr

                currHealth =
                    hero.health
            in
            if legalHeroMove board hero dr && hero.energy > 1 then
                if List.member HealthPotion (List.map (checkItemType newPos) board.item) then
                    { board | heroes = { hero | pos = newPos, energy = currEnergy - 2, health = currHealth + 10 } :: unselectedHero board.heroes, item = List.filter (\item -> item.pos /= newPos) board.item }

                else
                    { board | heroes = { hero | pos = newPos, energy = currEnergy - 2 } :: unselectedHero board.heroes }

            else
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
