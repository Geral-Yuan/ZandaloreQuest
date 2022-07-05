module EnemyAction exposing (actionEnemy)

import Board exposing (..)
import Data exposing (..)
import ShortestPath exposing (..)
import Action exposing (attackedByArcherRange)


actionEnemy : Board -> Board
actionEnemy board =
    let
        ( doneEnemy, undoneEnemy ) =
            List.partition .done board.enemies
    in
    case undoneEnemy of
        [] ->
            board

        enemy :: restEnemies ->
            let
                ( newenemy, newheroes ) =
                    actionSmartEnemy board enemy
            in
            { board | enemies = newenemy :: restEnemies ++ doneEnemy, heroes = newheroes }


actionSmartEnemy : Board -> Enemy -> ( Enemy, List Hero )
actionSmartEnemy board enemy =
    case enemy.class of
        Warrior ->
            actionSmartWarrior board enemy

        Archer ->
            actionSmartArcher board enemy

        Mage ->
            --ToDo some operations
            ( enemy, board.heroes )

        Assassin ->
            ( enemy, board.heroes )

        Healer ->
            ( enemy, board.heroes )


actionSmartWarrior : Board -> Enemy -> ( Enemy, List Hero )
actionSmartWarrior board enemy =
    let
        route =
            leastWarriorPath enemy board
    in
    case route of
        [] ->
            ( { enemy | done = True }
            , enemyWarriorAttack enemy board.heroes
                |> List.filter (\x -> x.health > 0)
            )

        first :: _ ->
            ( checkEnemyDone { enemy | steps = enemy.steps - 1, pos = first }, board.heroes )


enemyWarriorAttack : Enemy -> List Hero -> List Hero
enemyWarriorAttack enemy heroes =
    let
        ( attackableHeroes, restHeroes ) =
            List.partition (\hero -> List.member hero.pos (List.map (vecAdd enemy.pos) neighbour)) heroes

        sortedAttackableHeroes =
            List.sortBy .health attackableHeroes

        ( targetHero, newrestHeroes ) =
            case sortedAttackableHeroes of
                [] ->
                    ( [], heroes )

                hero :: otherHeroes ->
                    ( [ hero ], otherHeroes ++ restHeroes )
    in
    -- fix 0 for critical now
    List.map (\hero -> { hero | health = hero.health - enemy.damage - 0 }) targetHero ++ newrestHeroes


enemyArcherAttack : Enemy -> Board -> List Hero
enemyArcherAttack enemy board =
    let
        ( attackableHeroes, restHeroes ) =
            List.partition (\hero -> List.member hero.pos (attackedByArcherRange board enemy.pos)) board.heroes

        sortedAttackableHeroes =
            List.sortBy .health attackableHeroes

        ( targetHero, newrestHeroes ) =
            case sortedAttackableHeroes of
                [] ->
                    ( [], board.heroes )

                hero :: otherHeroes ->
                    ( [ hero ], otherHeroes ++ restHeroes )
    in
    -- fix 0 for critical now
    List.map (\hero -> { hero | health = hero.health - enemy.damage - 0 }) targetHero ++ newrestHeroes


actionSmartArcher : Board -> Enemy -> ( Enemy, List Hero )
actionSmartArcher board enemy =
    let
        route =
            -- leastArcherPath enemy board
            leastArcherPath enemy board
    in
    case route of
        [] ->
            ( { enemy | done = True }
            , enemyArcherAttack enemy board
                |> List.filter (\x -> x.health > 0)
            )

        first :: _ ->
            ( checkEnemyDone { enemy | steps = enemy.steps - 1, pos = first }, board.heroes )


checkEnemyDone : Enemy -> Enemy
checkEnemyDone enemy =
    if enemy.steps == 0 then
        { enemy | done = True }

    else
        enemy
