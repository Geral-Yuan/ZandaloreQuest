module HeroAttack exposing (checkAttack, generateDamage, selectedHero, unselectedHero)

import Board exposing (Board)
import Data exposing (..)
import AttackCommon exposing (isWarriorAttackRange)
import Message exposing (Msg(..))
import Random exposing (..)


randomDamage : Generator Critical
randomDamage =
    Random.weighted
        ( 10, Less )
        [ ( 15, None )
        , ( 25, Low )
        , ( 25, Medium )
        , ( 25, High )
        ]


generateDamage : Cmd Msg
generateDamage =
    Random.generate GetCritical randomDamage


checkAttack : Board -> Critical -> Board
checkAttack board critical =
    -- reduce the energy of a hero when player clicks h (hit) and check surroundings for enemies
    case selectedHero board.heroes of
        Nothing ->
            board

        Just hero ->
            if hero.energy > 2 then
                let
                    currEnergy =
                        hero.energy

                    newheroes =
                        { hero | energy = currEnergy - 3 } :: unselectedHero board.heroes

                    newcritical =
                        case critical of
                            Less ->
                                -2

                            Low ->
                                2

                            Medium ->
                                4

                            High ->
                                6

                            _ ->
                                0
                in
                checkForEnemy { board | critical = newcritical, heroes = newheroes }

            else
                board


checkForEnemy : Board -> Board
checkForEnemy board =
    -- check if there are enemies in the surroundings
    case selectedHero board.heroes of
        Nothing ->
            board

        Just hero ->
            case hero.class of
                Warrior ->
                    { board
                        | enemies =
                            List.map (checkMelee hero.pos hero.damage board.critical) board.enemies
                                |> List.filter (\{ health } -> health > 0)
                    }

                Assassin ->
                    { board
                        | enemies =
                            List.map (checkMelee hero.pos hero.damage board.critical) board.enemies
                                |> List.filter (\{ health } -> health > 0)
                    }

                _ ->
                    board


checkMelee : Pos -> Int -> Int -> Enemy -> Enemy
checkMelee ( x, y ) damage critical enemy =
    -- for warriors and assassins classes
    -- if there are enemies within the 6 hexagons around their current location
    -- the enemies will receive the damage
    let
        newHealth =
            enemy.health - damage - critical
    in
    if isWarriorAttackRange enemy.pos ( x, y ) then
        { enemy | health = newHealth }

    else
        enemy


selectedHero : List Hero -> Maybe Hero
selectedHero hero_list =
    List.head (List.filter (\hero -> hero.selected) hero_list)


unselectedHero : List Hero -> List Hero
unselectedHero hero_list =
    List.filter (\hero -> not hero.selected) hero_list
