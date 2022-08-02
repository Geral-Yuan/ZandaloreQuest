module EnemyAction exposing (actionEnemy, checkEnemyDone)

{-| This file fills functions related to all enemy actions.


# Functions

@docs actionEnemy, checkEnemyDone

-}

import Action exposing (attackedByArcherRange, attackedByMageRange, calculateHeal, checkAttackObstacle, pos2Item)
import ListOperation exposing (listDifference)
import ShortestPath exposing (leastArcherPath, leastHealerPath, leastMagePath, leastWarriorPath)
import Type exposing (Board, BoardState(..), Class(..), Enemy, Hero, HeroState(..), Obstacle, Pos)
import VectorOperation exposing (neighbour, subneighbour, vecAdd)


{-| check whether the enemy cannot do any action and update its state and justAttack
-}
checkEnemyDone : Enemy -> Enemy
checkEnemyDone enemy =
    if enemy.steps == 0 && enemy.state == Waiting then
        { enemy | done = True }

    else if enemy.justAttack then
        { enemy | justAttack = False, done = True }

    else
        enemy


{-| Let all undone enemies automaticly do their smart action and update the board with all influences enemies bring.
-}
actionEnemy : Board -> Board
actionEnemy board =
    let
        ( _, undoneEnemy ) =
            List.partition .done board.enemies
    in
    case undoneEnemy of
        [] ->
            board

        enemy :: _ ->
            actionSmartEnemy board enemy


actionSmartEnemy : Board -> Enemy -> Board
actionSmartEnemy board enemy =
    let
        nboard =
            case enemy.class of
                Warrior ->
                    actionSmartWarrior board enemy

                Archer ->
                    actionSmartArcher board enemy

                Mage ->
                    actionSmartMage board enemy

                Assassin ->
                    actionSmartWarrior board enemy

                Healer ->
                    actionSmartHealer board enemy

                Turret ->
                    case enemy.bossState of
                        1 ->
                            actionSmartWarrior { board | enemies = [ { enemy | bossState = 2 } ] } { enemy | bossState = 2 }

                        2 ->
                            actionSmartArcher { board | enemies = [ { enemy | bossState = 3 } ] } { enemy | bossState = 3 }

                        3 ->
                            actionSmartMage { board | enemies = [ { enemy | bossState = 4 } ] } { enemy | bossState = 4 }

                        _ ->
                            actionSmartHealer { board | enemies = [ { enemy | bossState = 1 } ] } { enemy | bossState = 1 }

                _ ->
                    board
    in
    nboard
        |> breakItem (index2Enemy enemy.indexOnBoard nboard.enemies)


actionSmartWarrior : Board -> Enemy -> Board
actionSmartWarrior board enemy =
    let
        route =
            leastWarriorPath enemy board

        other_enemies =
            listDifference board.enemies [ enemy ]
    in
    case route of
        [] ->
            { board
                | boardState = EnemyAttack
                , enemies = { enemy | justAttack = True, state = Attacking } :: other_enemies
                , heroes =
                    enemyWarriorAttack enemy board
                        |> checkHeroDeath
            }

        first :: _ ->
            { board
                | enemies = { enemy | steps = enemy.steps - 1, pos = first } :: other_enemies
            }


enemyWarriorAttack : Enemy -> Board -> List Hero
enemyWarriorAttack enemy board =
    let
        ( attackableHeroes, restHeroes ) =
            getEnemyAttackable enemy board

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
    List.map (heroAttacked enemy) targetHero ++ newrestHeroes


getEnemyAttackable : Enemy -> Board -> ( List Hero, List Hero )
getEnemyAttackable enemy board =
    case enemy.class of
        Warrior ->
            List.partition (\hero -> List.member hero.pos (List.map (vecAdd enemy.pos) neighbour)) board.heroes

        Archer ->
            List.partition (\hero -> List.member hero.pos (attackedByArcherRange board enemy.pos)) board.heroes

        Mage ->
            List.partition (\hero -> List.member hero.pos (attackedByMageRange enemy.pos)) board.heroes

        Assassin ->
            List.partition (\hero -> List.member hero.pos (List.map (vecAdd enemy.pos) neighbour)) board.heroes

        _ ->
            List.partition (\hero -> List.member hero.pos (List.map (vecAdd enemy.pos) neighbour)) board.heroes


enemyArcherAttack : Enemy -> Board -> List Hero
enemyArcherAttack enemy board =
    let
        ( attackableHeroes, restHeroes ) =
            getEnemyAttackable enemy board

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
    List.map (heroAttacked enemy) targetHero ++ newrestHeroes


actionSmartArcher : Board -> Enemy -> Board
actionSmartArcher board enemy =
    let
        route =
            -- leastArcherPath enemy board
            leastArcherPath enemy board

        otherenemies =
            listDifference board.enemies [ enemy ]
    in
    case route of
        [] ->
            { board
                | boardState = EnemyAttack
                , enemies = { enemy | justAttack = True, state = Attacking } :: otherenemies
                , heroes =
                    enemyArcherAttack enemy board
                        |> checkHeroDeath
            }

        first :: _ ->
            { board | enemies = { enemy | steps = enemy.steps - 1, pos = first } :: otherenemies }


actionSmartMage : Board -> Enemy -> Board
actionSmartMage board enemy =
    let
        route =
            leastMagePath enemy board

        otherenemies =
            listDifference board.enemies [ enemy ]

        atkboard =
            enemyMageAttack enemy board

        atkedheroes =
            atkboard.heroes
    in
    case route of
        [] ->
            { board
                | enemies = { enemy | justAttack = True, state = Attacking } :: otherenemies
                , heroes = atkedheroes |> checkHeroDeath
                , obstacles = atkboard.obstacles
                , item = atkboard.item
                , boardState = EnemyAttack
            }

        first :: _ ->
            { board | enemies = { enemy | steps = enemy.steps - 1, pos = first } :: otherenemies }


enemyMageAttack : Enemy -> Board -> Board
enemyMageAttack enemy board =
    let
        attackPlace =
            List.map (\x -> vecAdd x enemy.pos) subneighbour

        ( attackableHeroes, _ ) =
            getEnemyAttackable enemy board

        attackCombination =
            List.map (\tgt -> ( attackHeroGroup tgt attackableHeroes, tgt )) attackPlace

        sortedAttackableHeroes =
            List.sortBy (\x -> -1 * List.length (Tuple.first x)) attackCombination

        ( ( targetHero, newrestHeroes ), chosenGrid ) =
            case sortedAttackableHeroes of
                [] ->
                    ( ( [], [] ), ( -1, -1 ) )

                ( hero, grid ) :: _ ->
                    ( ( hero, listDifference board.heroes hero ), grid )

        newHeroes =
            List.map (heroAttacked enemy) targetHero ++ newrestHeroes

        tgtObsPos =
            attackObsGroup chosenGrid board.obstacles
                |> List.map .pos
    in
    -- fix 0 for critical now
    { board | heroes = newHeroes }
        |> checkAttackObstacle tgtObsPos


attackHeroGroup : Pos -> List Hero -> List Hero
attackHeroGroup grid attackable =
    List.filter (\x -> List.member x.pos (List.map (vecAdd grid) (( 0, 0 ) :: neighbour))) attackable


attackObsGroup : Pos -> List Obstacle -> List Obstacle
attackObsGroup grid attackable =
    List.filter (\x -> List.member x.pos (List.map (vecAdd grid) (( 0, 0 ) :: neighbour))) attackable


actionSmartHealer : Board -> Enemy -> Board
actionSmartHealer board enemy =
    let
        route =
            if isOnlyEnemyHealer board || isAllMaxHealth board then
                leastWarriorPath enemy board

            else
                leastHealerPath enemy board

        otherenemies =
            listDifference board.enemies [ enemy ]

        ( healedhealer, healedenemies ) =
            enemyHeal enemy otherenemies

        atkboard =
            if isOnlyEnemyHealer board || isAllMaxHealth board then
                { board | heroes = enemyWarriorAttack enemy board }

            else
                board

        atkedheroes =
            atkboard.heroes
    in
    case route of
        [] ->
            let
                new_state =
                    case healedhealer.state of
                        GettingHealed _ ->
                            healedhealer.state

                        _ ->
                            Attacking

                new_healedhealer =
                    { healedhealer
                        | justAttack = True
                        , state = new_state
                    }
            in
            { board
                | enemies = new_healedhealer :: healedenemies
                , heroes = atkedheroes |> checkHeroDeath
                , obstacles = atkboard.obstacles
                , item = atkboard.item
                , boardState = EnemyAttack
            }

        first :: _ ->
            { board | enemies = { enemy | steps = enemy.steps - 1, pos = first } :: otherenemies }


enemyHeal : Enemy -> List Enemy -> ( Enemy, List Enemy )
enemyHeal enemy healed =
    let
        ( healable, others ) =
            List.partition (\hero -> List.member hero.pos (List.map (vecAdd enemy.pos) neighbour) && (hero.health /= hero.maxHealth)) healed

        sortedAttackableHeroes =
            List.sortBy .health healable

        ( targetHealed, newothers ) =
            case sortedAttackableHeroes of
                [] ->
                    ( [], others )

                chosen :: othernothealed ->
                    ( [ chosen ], othernothealed ++ others )
    in
    if (enemy.health /= enemy.maxHealth) && List.all (\x -> x.health > enemy.health) targetHealed then
        ( getHealed enemy enemy, healed )

    else
        ( enemy, List.map (getHealed enemy) targetHealed ++ newothers )


getHealed : Enemy -> Enemy -> Enemy
getHealed healer_enemy hdenemy =
    { hdenemy
        | health = hdenemy.health + addhealth healer_enemy hdenemy
        , state = GettingHealed (addhealth healer_enemy hdenemy)
    }


isOnlyEnemyHealer : Board -> Bool
isOnlyEnemyHealer board =
    List.all (\enemy -> enemy.class == Healer) board.enemies


isAllMaxHealth : Board -> Bool
isAllMaxHealth board =
    List.all (\enemy -> enemy.health >= enemy.maxHealth) board.enemies


addhealth : Enemy -> Enemy -> Int
addhealth enemy hdenemy =
    if enemy == hdenemy then
        min (hdenemy.maxHealth - hdenemy.health) (calculateHeal enemy.damage + 5)

    else
        min (hdenemy.maxHealth - hdenemy.health) (calculateHeal enemy.damage)


checkHeroDeath : List Hero -> List Hero
checkHeroDeath list_hero =
    List.filter (\hero -> hero.health > 0) list_hero


breakItem : Enemy -> Board -> Board
breakItem enemy board =
    let
        chosenItem =
            pos2Item board.item enemy.pos

        otherItems =
            listDifference board.item [ chosenItem ]
    in
    { board | item = otherItems }


index2Enemy : Int -> List Enemy -> Enemy
index2Enemy index l_enemy =
    case List.filter (\x -> index == x.indexOnBoard) l_enemy of
        [] ->
            Enemy Warrior ( 0, 0 ) 80 -1 15 3 False Waiting False 0 False 0

        chosen :: _ ->
            chosen


heroAttacked : Enemy -> Hero -> Hero
heroAttacked enemy hero =
    { hero | health = hero.health - enemy.damage - 0, state = Attacked enemy.damage }
