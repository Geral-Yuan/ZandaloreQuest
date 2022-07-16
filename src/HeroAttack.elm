module HeroAttack exposing (checkAttack, generateDamage)

import Action exposing (checkAttackObstacle, selectedHero, unselectedHero, checkBuildObstacle)
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
            if hero.energy > 2 && isMeaningfulAttack board hero.class pos then
                --    if hero.energy > 2 && List.member pos (listIntersection (List.map .pos board.enemies ++ List.map .pos board.obstacles) board.attackable) then
                let
                    chosenclass = hero.class
                    newheroes =
                        { hero | energy = hero.energy - 3, state = Attacking } :: unselectedHero board.heroes

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

                    attackedPoslist =
                        case hero.class of
                            Mage ->
                                pos :: List.map (vecAdd pos) neighbour

                            _ ->
                                [ pos ]
                in
                List.foldl (checkAttackTarget chosenclass) { board | critical = newcritical, heroes = newheroes, turn = AttackInProgress } attackedPoslist

            else
                board


isMeaningfulAttack : Board -> Class -> Pos -> Bool
isMeaningfulAttack board class pos =
    case class of
        Mage ->
            List.member pos (listIntersection board.attackable (extentPos (meaningfulTarget board class) (( 0, 0 ) :: neighbour)))

        _ ->
            List.member pos (listIntersection board.attackable (meaningfulTarget board class))


meaningfulTarget : Board -> Class -> List Pos
meaningfulTarget board class =
    case class of
        Engineer ->
            listDifference board.map (List.map .pos (List.filter (\obstacle -> obstacle.obstacleType == Unbreakable) board.obstacles)
                                        ++ List.map .pos board.heroes
                                        ++ List.map .pos board.item)
        
        _ ->
            List.map .pos board.enemies ++ List.map .pos (List.filter (\obstacle -> obstacle.obstacleType == MysteryBox) board.obstacles)





checkAttackTarget : Class -> Pos -> Board -> Board
checkAttackTarget class pos board =
    board
        |> checkAttackObstacle [ pos ]
        |> checkAttackEnemy pos
        |> (checkBuildObstacle class pos)
        -- |> (checkHeal class pos)



{-
   checkAttackTarget : Pos -> Board -> Board
   checkAttackTarget pos board =
       if List.member MysteryBox (List.map (checkObstacleType pos) board.obstacles) || List.member Unbreakable (List.map (checkObstacleType pos) board.obstacles) then
           checkAttackObstacle board pos

       else
           checkAttackEnemy board pos
-}


checkAttackEnemy : Pos -> Board -> Board
checkAttackEnemy pos board =
    case selectedHero board.heroes of
        Nothing ->
            board

        Just hero ->
            let
                ( attackedEnemies, otherEnemies ) =
                    List.partition (\enemy -> enemy.pos == pos) board.enemies
            in
            { board | enemies = List.filter (\{ health } -> health > 0) (List.map (damageEnemy hero.damage board.critical) attackedEnemies ++ otherEnemies) }


damageEnemy : Int -> Int -> Enemy -> Enemy
damageEnemy damage critical enemy =
    { enemy | health = enemy.health - damage - critical, state = Attacked (critical + damage) }



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
