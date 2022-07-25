module EnemyAction exposing (actionEnemy, checkEnemyDone)

import Action exposing (attackedByArcherRange, attackedByMageRange, calculateHeal, checkAttackObstacle, pos2Item)
import Board exposing (Board)
import Data exposing (..)
import ShortestPath exposing (leastArcherPath, leastHealerPath, leastMagePath, leastWarriorPath)


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
                    board

                Healer ->
                    actionSmartHealer board enemy

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

        otherenemies =
            listDifference board.enemies [ enemy ]
    in
    case route of
        [] ->
            eh2Board
                ( { enemy | justAttack = True, state = Attacking } :: otherenemies
                , enemyWarriorAttack enemy board.heroes
                    |> List.filter (\x -> x.health > 0)
                )
                { board | boardState = EnemyAttack }

        first :: _ ->
            eh2Board
                ( { enemy | steps = enemy.steps - 1, pos = first } :: otherenemies
                , board.heroes
                )
                board


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
    List.map (\hero -> { hero | health = hero.health - enemy.damage - 0, state = Attacked enemy.damage }) targetHero ++ newrestHeroes


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
    List.map (\hero -> { hero | health = hero.health - enemy.damage - 0, state = Attacked enemy.damage }) targetHero ++ newrestHeroes


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
            eh2Board
                ( { enemy | justAttack = True, state = Attacking } :: otherenemies
                , enemyArcherAttack enemy board
                    |> List.filter (\x -> x.health > 0)
                )
                { board | boardState = EnemyAttack }

        first :: _ ->
            eh2Board
                ( { enemy | steps = enemy.steps - 1, pos = first } :: otherenemies
                , board.heroes
                )
                board


actionSmartMage : Board -> Enemy -> Board
actionSmartMage board enemy =
    let
        route =
            -- leastArcherPath enemy board
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
                , heroes = atkedheroes |> List.filter (\x -> x.health > 0)
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

        -- |> List.partition (\x -> List.member x (List.map .pos board.obstacles))
        ( attackableHeroes, _ ) =
            List.partition (\hero -> List.member hero.pos (attackedByMageRange enemy.pos)) board.heroes

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
            List.map (\hero -> { hero | health = hero.health - enemy.damage - 0, state = Attacked enemy.damage }) targetHero ++ newrestHeroes

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

        healedenemies =
            enemyHeal enemy otherenemies

        atkboard =
            if isOnlyEnemyHealer board || isAllMaxHealth board then
                { board | heroes = enemyWarriorAttack enemy board.heroes }

            else
                board

        atkedheroes =
            atkboard.heroes
    in
    case route of
        [] ->
            { board
                | enemies = { enemy | justAttack = True, state = Attacking } :: healedenemies
                , heroes = atkedheroes |> List.filter (\x -> x.health > 0)
                , obstacles = atkboard.obstacles
                , item = atkboard.item
                , boardState = EnemyAttack
            }

        first :: _ ->
            { board | enemies = { enemy | steps = enemy.steps - 1, pos = first } :: otherenemies }


enemyHeal : Enemy -> List Enemy -> List Enemy
enemyHeal enemy healed =
    let
        ( healable, others ) =
            List.partition (\hero -> List.member hero.pos (List.map (vecAdd enemy.pos) neighbour)) healed

        sortedAttackableHeroes =
            List.sortBy .health healable

        ( targetHealed, newothers ) =
            case sortedAttackableHeroes of
                [] ->
                    ( [], others )

                chosen :: othernothealed ->
                    ( [ chosen ], othernothealed ++ others )
    in
    -- fix 0 for critical now
    List.map
        (\hdenemy ->
            { hdenemy
                | health = hdenemy.health + addhealth enemy hdenemy
                , state = GettingHealed (addhealth enemy hdenemy)
            }
        )
        targetHealed
        ++ newothers


isOnlyEnemyHealer : Board -> Bool
isOnlyEnemyHealer board =
    List.all (\enemy -> enemy.class == Healer) board.enemies


isAllMaxHealth : Board -> Bool
isAllMaxHealth board =
    List.all (\enemy -> enemy.health >= enemy.maxHealth) board.enemies


addhealth : Enemy -> Enemy -> Int
addhealth enemy hdenemy =
    min (hdenemy.maxHealth - hdenemy.health) (calculateHeal enemy.damage)


checkEnemyDone : Enemy -> Enemy
checkEnemyDone enemy =
    if enemy.steps == 0 && enemy.state == Waiting then
        { enemy | done = True }

    else if enemy.justAttack then
        { enemy | justAttack = False, done = True }

    else
        enemy



-- checkEHDeath : Board -> Board
-- checkEHDeath board =
--     let
--         nenemy = List.filter (\x -> x.health > 0) board.enemies
--         nheroes = List.filter (\x -> x.health > 0) board.heroes
--     in
--     {board | heroes = nheroes, enemies = nenemy}


breakItem : Enemy -> Board -> Board
breakItem enemy board =
    let
        chosenItem =
            pos2Item board.item enemy.pos

        otherItems =
            listDifference board.item [ chosenItem ]
    in
    { board | item = otherItems }


eh2Board : ( List Enemy, List Hero ) -> Board -> Board
eh2Board ( l_enemy, l_hero ) board =
    { board | enemies = l_enemy, heroes = l_hero }


index2Enemy : Int -> List Enemy -> Enemy
index2Enemy index l_enemy =
    case List.filter (\x -> index == x.indexOnBoard) l_enemy of
        [] ->
            Enemy Warrior ( 0, 0 ) 80 -1 15 3 False Waiting False 0

        chosen :: _ ->
            chosen
