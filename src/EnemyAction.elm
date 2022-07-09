module EnemyAction exposing (actionEnemy)

import Action exposing (attackedByArcherRange, attackedByMageRange)
import Board exposing (..)
import Data exposing (..)
import ShortestPath exposing (..)


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
            actionSmartMage board enemy

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


actionSmartMage : Board -> Enemy -> ( Enemy, List Hero )
actionSmartMage board enemy =
    let
        route =
            -- leastArcherPath enemy board
            leastMagePath enemy board
    in
    case route of
        [] ->
            ( { enemy | done = True }
            , enemyMageAttack enemy board
                |> List.filter (\x -> x.health > 0)
            )

        first :: _ ->
            ( checkEnemyDone { enemy | steps = enemy.steps - 1, pos = first }, board.heroes )


enemyMageAttack : Enemy -> Board -> List Hero
enemyMageAttack enemy board =
    let
        ( _, attackGrids ) =
            List.map (\x -> vecAdd x enemy.pos) subneighbour
                |> List.partition (\x -> List.member x (List.map .pos board.obstacles))

        ( attackableHeroes, restHeroes ) =
            List.partition (\hero -> List.member hero.pos (attackedByMageRange enemy.pos)) board.heroes

        attackCombination =
            List.map (\tgt -> attackGroup tgt attackableHeroes) attackGrids

        sortedAttackableHeroes =
            List.sortBy (\x -> -1 * List.length x) attackCombination

        ( targetHero, newrestHeroes ) =
            case sortedAttackableHeroes of
                [] ->
                    ( [], board.heroes )

                [ [] ] ->
                    ( [], board.heroes )

                hero :: otherHeroes ->
                    ( hero, List.concatMap (\x -> listDifference x hero) otherHeroes ++ restHeroes )
    in
    -- fix 0 for critical now
    List.map (\hero -> { hero | health = hero.health - enemy.damage - 0 }) targetHero ++ newrestHeroes


attackGroup : Pos -> List Hero -> List Hero
attackGroup grid attackable =
    List.filter (\hero -> List.member hero.pos (List.map (vecAdd grid) (( 0, 0 ) :: neighbour))) attackable


checkEnemyDone : Enemy -> Enemy
checkEnemyDone enemy =
    if enemy.steps == 0 then
        { enemy | done = True }

    else
        enemy
