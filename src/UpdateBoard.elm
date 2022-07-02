module UpdateBoard exposing (updateBoard)


import Board exposing (Board)
import Data exposing (..)
import Message exposing (Msg(..))
import HeroAttack exposing (checkAttack, selectedHero, unselectedHero)
import EnemyAction exposing (..)

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
            let
                nboard =
                    highlightCells board
            in
            case board.turn of
                HeroTurn ->
                    nboard

                EnemyTurn ->
                    { nboard | time = nboard.time + elapsed / 1000 }
                        |> actionEnemy
                        |> checkTurn

        GetCritical critical ->
            checkAttack board critical

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

legalHeroMove : Board -> Hero -> Pos -> Bool
legalHeroMove board hero dr =
    List.member (vecAdd hero.pos dr) board.map && not (List.member (vecAdd hero.pos dr) (board.barrier ++ List.map .pos board.heroes ++ List.map .pos board.enemies))


highlightCells : Board -> Board
highlightCells board =
    board
        |> heroMoveable
        |> heroAttackable


heroMoveable : Board -> Board
heroMoveable board =
    let
        sample_hero =
            Hero Warrior ( 100, 100 ) 1 1 1 1 False 100

        selected =
            selectedHero board.heroes
                |> Maybe.withDefault sample_hero

        {- neighbour =
           vecAdd selected.pos ( 1, 0 )
        -}
        neighbour =
            List.map (vecAdd selected.pos) [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]

        can_move =
            neighbour
                |> List.filter (myListMember "in" board.map)
                |> List.filter (myListMember "not in" board.barrier)
                |> List.filter (myListMember "not in" (List.map .pos board.heroes))
                |> List.filter (myListMember "not in" (List.map .pos board.enemies))
    in
    { board | moveable = can_move }


heroAttackable : Board -> Board
heroAttackable board =
    let
        sample_hero =
            Hero Warrior ( 100, 100 ) 1 1 1 1 False 100

        selected =
            selectedHero board.heroes
                |> Maybe.withDefault sample_hero

        neighbour =
            [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]
    in
    case selected.class of
        Warrior ->
            let
                attack_range =
                    List.map (vecAdd selected.pos) neighbour

                can_attack =
                    attack_range
                        |> List.filter (myListMember "in" board.map)
                        |> List.filter (myListMember "not in" board.barrier)
                        |> List.filter (myListMember "not in" (List.map .pos board.heroes))
                        |> List.filter (myListMember "in" (List.map .pos board.enemies))
            in
            { board | attackable = can_attack }

        Assassin ->
            let
                attack_range =
                    List.map (vecAdd selected.pos) neighbour

                can_attack =
                    attack_range
                        |> List.filter (myListMember "in" board.map)
                        |> List.filter (myListMember "not in" board.barrier)
                        |> List.filter (myListMember "not in" (List.map .pos board.heroes))
                        |> List.filter (myListMember "in" (List.map .pos board.enemies))
            in
            { board | attackable = can_attack }

        Archer ->
            board

        {- let
               attack_range =
                   List.map (vecAdd selected.pos) (cartesianProduct vecScale (List.range 1 10) neighbour)

               can_attack =
                   attack_range
                       |> List.filter (myListMember "in" board.map)
                       |> List.filter (myListMember "not in" board.barrier)
                       |> List.filter (myListMember "not in" (List.map .pos hero_list))
                       |> List.filter (myListMember "in" (List.map .pos board.enemies))
           in
           { board | attackable = can_attack }
        -}
        Mage ->
            board


myListMember : String -> List a -> a -> Bool
myListMember s lis x =
    if s == "in" then
        List.member x lis

    else
        not (List.member x lis)



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