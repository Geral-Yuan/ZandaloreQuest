module EnemyAction exposing (actionEnemy)

import Action exposing (attackedByArcherRange, attackedByMageRange, checkAttackObstacle, pos2Item)
import Board exposing (..)
import Data exposing (..)
import ShortestPath exposing (..)


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
            (actionSmartEnemy board enemy)


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
                    board
    in
    nboard
    |> breakItem (index2Enemy enemy.indexOnBoard nboard.enemies)

actionSmartWarrior : Board -> Enemy -> Board
actionSmartWarrior board enemy =
    let
        route =
            leastWarriorPath enemy board

        otherenemies = listDifference board.enemies [enemy]
    in
    case route of
        [] ->
            eh2Board 
            ( { enemy | done = True } :: otherenemies
            , enemyWarriorAttack enemy board.heroes
                |> List.filter (\x -> x.health > 0)
            ) board

        first :: _ ->
            eh2Board 
            ( (checkEnemyDone { enemy | steps = enemy.steps - 1, pos = first }) :: otherenemies
            , board.heroes ) board


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


actionSmartArcher : Board -> Enemy -> Board
actionSmartArcher board enemy =
    let
        route =
            -- leastArcherPath enemy board
            leastArcherPath enemy board

        otherenemies = listDifference board.enemies [enemy]
    in
    case route of
        [] ->
            eh2Board
            ( { enemy | done = True } :: otherenemies
            , enemyArcherAttack enemy board
                |> List.filter (\x -> x.health > 0)
            ) board

        first :: _ ->
            eh2Board
            ( (checkEnemyDone { enemy | steps = enemy.steps - 1, pos = first }) :: otherenemies
            , board.heroes ) board


actionSmartMage : Board -> Enemy -> Board
actionSmartMage board enemy =
    let
        route =
            -- leastArcherPath enemy board
            leastMagePath enemy board


        otherenemies = listDifference board.enemies [enemy]

        atkboard = enemyMageAttack enemy board

        atkedheroes = atkboard.heroes
    in
    case route of
        [] ->
            {board | enemies = { enemy | done = True } :: otherenemies
            , heroes = atkedheroes |> List.filter (\x -> x.health > 0)
            , obstacles = atkboard.obstacles
            , item = atkboard.item
            }

        first :: _ ->
            {board | enemies = (checkEnemyDone { enemy | steps = enemy.steps - 1, pos = first }) :: otherenemies }



enemyMageAttack : Enemy -> Board -> Board
enemyMageAttack enemy board =
    let
        attackPlace =
            List.map (\x -> vecAdd x enemy.pos) subneighbour
                -- |> List.partition (\x -> List.member x (List.map .pos board.obstacles))

        ( attackableHeroes, restHeroes ) =
            List.partition (\hero -> List.member hero.pos (attackedByMageRange enemy.pos)) board.heroes
           

        attackCombination =
            List.map (\tgt -> (attackHeroGroup tgt attackableHeroes, tgt)) attackPlace

        sortedAttackableHeroes =
            List.sortBy (\x -> -1 * List.length (Tuple.first x)) attackCombination

        (( targetHero, newrestHeroes ), chosenGrid) =
            case sortedAttackableHeroes of
                [] ->
                    (( [], [] ), (-1, -1))

                (hero, grid) :: otherHeroes ->
                    (( hero, List.concatMap (\x -> listDifference (Tuple.first x) hero) (otherHeroes) ++ restHeroes ), grid)

        newHeroes = List.map (\hero -> { hero | health = hero.health - enemy.damage - 0 }) targetHero ++ newrestHeroes
        
        tgtObsPos = attackObsGroup chosenGrid board.obstacles
                    |> List.map .pos


    in
    -- fix 0 for critical now
    {board | heroes = newHeroes}
    |> checkAttackObstacle tgtObsPos


attackHeroGroup : Pos -> List Hero -> List Hero
attackHeroGroup grid attackable =
    List.filter (\x -> List.member x.pos (List.map (vecAdd grid) (( 0, 0 ) :: neighbour))) attackable


attackObsGroup : Pos -> List Obstacle -> List Obstacle
attackObsGroup grid attackable =
    List.filter (\x -> List.member x.pos (List.map (vecAdd grid) (( 0, 0 ) :: neighbour))) attackable


checkEnemyDone : Enemy -> Enemy
checkEnemyDone enemy =
    if enemy.steps == 0 then
        { enemy | done = True }

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
        chosenItem = pos2Item board.item enemy.pos
        otherItems = listDifference board.item [chosenItem]
    in
        { board |  item = otherItems }

eh2Board : (List Enemy, List Hero) -> Board -> Board
eh2Board (l_enemy, l_hero) board =
    {board | enemies = l_enemy, heroes = l_hero}

index2Enemy : Int -> List Enemy -> Enemy
index2Enemy index l_enemy =
    case List.filter (\x -> (index == x.indexOnBoard)) l_enemy of
        [] ->
            Enemy Warrior ( 0, 0 ) -1 15 5 3 False 0
        chosen :: _ ->
            chosen

