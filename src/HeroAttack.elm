module HeroAttack exposing (checkAttack, generateDamage, selectedHero, unselectedHero)

import Board exposing (..)
import Data exposing (..)
import Message exposing (Critical(..), Msg(..))
import Model exposing (Model)
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


checkAttack : Model -> Critical -> Model
checkAttack model critical =
    -- reduce the energy of a hero when player clicks h (hit) and check surroundings for enemies
    case selectedHero model.heroes of
        Nothing ->
            model

        Just hero ->
            if hero.energy > 2 then
                let
                    currEnergy =
                        hero.energy
                in
                case critical of
                    Less ->
                        { model | critical = -2, heroes = { hero | energy = currEnergy - 3 } :: unselectedHero model.heroes, board = checkForEnemy model }

                    Low ->
                        { model | critical = 2, heroes = { hero | energy = currEnergy - 3 } :: unselectedHero model.heroes, board = checkForEnemy model }

                    Medium ->
                        { model | critical = 4, heroes = { hero | energy = currEnergy - 3 } :: unselectedHero model.heroes, board = checkForEnemy model }

                    High ->
                        { model | critical = 6, heroes = { hero | energy = currEnergy - 3 } :: unselectedHero model.heroes, board = checkForEnemy model }

                    _ ->
                        { model | critical = 0, heroes = { hero | energy = currEnergy - 3 } :: unselectedHero model.heroes, board = checkForEnemy model }

            else
                model


checkForEnemy : Model -> Board
checkForEnemy model =
    -- check if there are enemies in the surroundings
    case selectedHero model.heroes of
        Nothing ->
            model.board

        Just hero ->
            let
                board =
                    model.board
            in
            case hero.class of
                Warrior ->
                    { board
                        | enemies =
                            List.map (checkMelee hero.pos hero.damage model.critical) model.board.enemies
                                |> List.filter (\{ health } -> health > 0)
                    }

                Assassin ->
                    { board
                        | enemies =
                            List.map (checkMelee hero.pos hero.damage model.critical) model.board.enemies
                                |> List.filter (\{ health } -> health > 0)
                    }

                _ ->
                    model.board


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
