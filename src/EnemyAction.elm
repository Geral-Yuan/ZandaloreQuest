module EnemyAction exposing (actionEnemy, actionEnemyList, isArcherAttackRange)

import Board exposing (..)
import Data exposing (..)
import ShortestPath exposing (..)

actionEnemy : Board -> Board
actionEnemy board =
    let
        ( newEnemy, newboard ) =
            actionEnemyList board board.enemies
    in
    { newboard | enemies = newEnemy }


actionEnemyList : Board -> List Enemy -> ( List Enemy, Board )
actionEnemyList board enemy_list =
    case enemy_list of
        [] ->
            ( [], board )

        enemy :: restEnemy ->
            let
                ( movedEnemy, newboard ) =
                    --moveOneEnemy model enemy
                    actionSmartEnemy board enemy

                ( newrestEnemy, nnewboard ) =
                    actionEnemyList newboard restEnemy
            in
            ( movedEnemy :: newrestEnemy, nnewboard )


actionSmartEnemy : Board -> Enemy -> ( Enemy, Board )
actionSmartEnemy board enemy =
    case enemy.class of
        Warrior ->
            actionSmartWarrior board enemy
        Archer ->
             actionSmartArcher board enemy
        Mage ->
            --ToDo some operations
            ( enemy, board )
        Assassin ->
            ( enemy, board )
        Healer ->
            ( enemy, board )


actionSmartWarrior : Board -> Enemy -> ( Enemy, Board )
actionSmartWarrior board enemy =
    let
        route =
            leastWarriorPath enemy board board.heroes
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


enermyArcherAttack : Board -> Enemy -> Int -> Int -> Hero -> Hero
enermyArcherAttack board my_enemy damage critical hero =
    let
        newHealth =
            hero.health - damage - critical
    in
    if isArcherAttackRange board hero my_enemy then
        { hero | health = newHealth }

    else
        hero


isArcherAttackRange : Board -> Hero -> Enemy -> Bool
isArcherAttackRange board attacked me =
    if
        isBetweenEmpty board me attacked.pos me.pos
    then
        True

    else
        False



actionSmartArcher : Board -> Enemy -> ( Enemy, Board )
actionSmartArcher board enemy =
    let
        route =
            leastArcherPath enemy board
    in
    case route of
        [] ->
            if not enemy.done then
                ( { enemy | done = True }
                , { board
                    | time = 0
                    , heroes =
                        List.map (enermyArcherAttack board enemy 7 0) board.heroes
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




checkEnemyDone : Enemy -> Enemy

checkEnemyDone enemy =
    if enemy.steps == 0 then
        { enemy | done = True }

    else
        enemy