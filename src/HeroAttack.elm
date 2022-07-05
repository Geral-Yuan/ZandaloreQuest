module HeroAttack exposing (checkAttack, generateDamage)

import Action exposing (checkObstacleType, selectedHero, unselectedHero)
import Board exposing (Board)
import Data exposing (..)
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


generateDamage : Pos -> Cmd Msg
generateDamage pos =
    Random.generate (Attack pos) randomDamage


checkAttack : Board -> Pos -> Critical -> Board
checkAttack board pos critical =
    -- reduce the energy of a hero when player clicks h (hit) and check surroundings for enemies
    case selectedHero board.heroes of
        Nothing ->
            board

        Just hero ->
            if hero.energy > 2 && List.member pos (listIntersection (List.map .pos board.enemies ++ List.map .pos board.obstacles) board.attackable) then
                let
                    newheroes =
                        { hero | energy = hero.energy - 3 } :: unselectedHero board.heroes

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
                checkAttackTarget { board | critical = newcritical, heroes = newheroes } pos

            else
                board


checkAttackTarget : Board -> Pos -> Board
checkAttackTarget board pos =
    if List.member MysteryBox (List.map (checkObstacleType pos) board.obstacles) || List.member Unbreakable (List.map (checkObstacleType pos) board.obstacles) then
        checkAttackBarrier board pos

    else
        checkAttackEnemy board pos


checkAttackBarrier : Board -> Pos -> Board
checkAttackBarrier board position =
    case selectedHero board.heroes of
        Nothing ->
            board

        Just hero ->
            let
                ( attacked, others ) =
                    List.partition (\barrier -> barrier.pos == position) board.obstacles

                ( attackBreakable, attackOthers ) =
                    List.partition (\barrier -> barrier.obstacleType == MysteryBox) attacked
            in
            case hero.class of
                -- Mage ->
                --     { board | obstacles = List.filter (\bpos -> List.member bpos (pos :: List.map (vecAdd pos) neighbour)) board.obstacles }
                _ ->
                    { board | obstacles = attackOthers ++ others, item = List.map (\obstacle -> Item obstacle.itemType obstacle.pos) attackBreakable ++ board.item }


checkAttackEnemy : Board -> Pos -> Board
checkAttackEnemy board pos =
    case selectedHero board.heroes of
        Nothing ->
            board

        Just hero ->
            let
                ( attackedEnemies, otherEnemies ) =
                    case hero.class of
                        Mage ->
                            List.partition (\enemy -> List.member enemy.pos (pos :: List.map (vecAdd pos) neighbour)) board.enemies

                        _ ->
                            List.partition (\enemy -> enemy.pos == pos) board.enemies
            in
            { board | enemies = List.filter (\{ health } -> health > 0) (List.map (damageEnemy hero.damage board.critical) attackedEnemies ++ otherEnemies) }


damageEnemy : Int -> Int -> Enemy -> Enemy
damageEnemy damage critical enemy =
    { enemy | health = enemy.health - damage - critical }



{-
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

                   Healer ->
                       { board
                           | heroes =
                               List.map (checkHeal hero.pos hero.damage board.critical) board.heroes
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


   checkHeal : Pos -> Int -> Int -> Hero -> Hero
   checkHeal ( x, y ) heal critical hero =
       -- for warriors and assassins classes
       -- if there are enemies within the 6 hexagons around their current location
       -- the enemies will receive the damage
       let
           newHealth =
               hero.health + heal + critical
       in
       if isWarriorAttackRange hero.pos ( x, y ) then
           { hero | health = newHealth }

       else
           hero
-}
