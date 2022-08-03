module Action exposing (attackedByArcherRange, attackedByHeroArcherRange, attackedByMageRange, calculateHeal, checkAttackObstacle, checkBuildTurret, checkHeal, index2Hero, pos2Item, selectedHero, unMoveable, unselectedHero, updateAttackable, updateEnemyAttackable, updateTarget)

{-| This file fills functions related to all heroes actions.


# Functions

@docs attackedByArcherRange, attackedByHeroArcherRange, attackedByMageRange, calculateHeal, checkAttackObstacle, checkBuildTurret, checkHeal, index2Hero, pos2Item, selectedHero, unMoveable, unselectedHero, updateAttackable, updateEnemyAttackable, updateTarget

-}

import Data exposing (findHexagon)
import ListOperation exposing (listDifference, listIntersection, listUnion)
import Message exposing (Msg(..))
import Type exposing (Board, BoardState(..), Class(..), Enemy, FailToDo(..), Hero, HeroState(..), Item, ItemType(..), ObstacleType(..), Pos, Side(..), Turn(..))
import VectorOperation exposing (leastdistance, neighbour, sameline, subneighbour, subsubneighbour, vecAdd, vecScale)


{-| This function will update the attackable position according to the enemy's position
-}
updateEnemyAttackable : Board -> Board
updateEnemyAttackable board =
    let
        maybeEnemy =
            List.head (List.filter (\x -> x.indexOnBoard == board.cntEnemy) board.enemies)
    in
    if board.turn == EnemyTurn then
        case maybeEnemy of
            Nothing ->
                { board | enemyAttackable = [] }

            Just enemy ->
                let
                    realattackRange =
                        List.map (vecAdd enemy.pos) (attackRangeEnemy board enemy)
                in
                { board | enemyAttackable = realattackRange }

    else
        board


{-| This function will update the attackable position according to the hero's position.
-}
updateAttackable : Board -> Board
updateAttackable board =
    case selectedHero board.heroes of
        Nothing ->
            { board | attackable = [], skillable = [] }

        Just hero ->
            let
                realattackRange =
                    if hero.class /= Turret then
                        List.map (vecAdd hero.pos) (attackRange board hero)

                    else
                        []

                realskillRange =
                    if hero.class /= Turret then
                        case hero.class of
                            Healer ->
                                List.map (vecAdd hero.pos) (skillRange hero) |> listIntersection (List.map .pos board.heroes)

                            Engineer ->
                                List.map (vecAdd hero.pos) (skillRange hero) |> List.filter (\x -> isEngineerSkillGrid x board)

                            _ ->
                                []

                    else
                        []
            in
            { board | attackable = realattackRange, skillable = realskillRange }


attackRange : Board -> Hero -> List Pos
attackRange board hero =
    case hero.class of
        Archer ->
            List.concatMap (stuckInWay board hero.pos Friend) neighbour

        Turret ->
            List.concatMap (stuckInWay board hero.pos Friend) neighbour

        Mage ->
            subneighbour

        _ ->
            neighbour


attackRangeEnemy : Board -> Enemy -> List Pos
attackRangeEnemy board enemy =
    case enemy.class of
        Archer ->
            List.concat (List.map (stuckInWay board enemy.pos Hostile) neighbour)

        Mage ->
            subneighbour

        Healer ->
            ( 0, 0 ) :: neighbour

        Turret ->
            case enemy.bossState of
                2 ->
                    List.concat (List.map (stuckInWay board enemy.pos Hostile) neighbour)

                3 ->
                    subneighbour

                4 ->
                    ( 0, 0 ) :: neighbour

                _ ->
                    neighbour

        _ ->
            neighbour


skillRange : Hero -> List Pos
skillRange hero =
    case hero.class of
        Engineer ->
            neighbour ++ subneighbour

        Healer ->
            ( 0, 0 ) :: neighbour

        _ ->
            []


{-| This function will return List of Positions that can be hit by Archer on the Board.
-}
attackedByArcherRange : Board -> Pos -> List Pos
attackedByArcherRange board pos =
    List.map (vecAdd pos) (List.concat (List.map (stuckInWay board pos Hostile) neighbour))


{-| This function will return List of Positions that can be hit by Hero Archer on the Board.
-}
attackedByHeroArcherRange : Board -> Pos -> List Pos
attackedByHeroArcherRange board pos =
    List.map (vecAdd pos) (List.concat (List.map (stuckInWay board pos Friend) neighbour))


stuckInWay : Board -> Pos -> Side -> Pos -> List Pos
stuckInWay board my_pos my_side nbhd_pos =
    let
        linePos =
            List.map (vecAdd my_pos) (sameline nbhd_pos)

        inWay =
            case my_side of
                Friend ->
                    listIntersection linePos (List.map .pos board.obstacles ++ List.map .pos board.enemies)

                Hostile ->
                    listIntersection linePos (List.map .pos board.obstacles ++ List.map .pos board.heroes)
    in
    case leastdistance inWay my_pos of
        Nothing ->
            sameline nbhd_pos

        Just dis ->
            List.map (\k -> vecScale k nbhd_pos) (List.range 1 dis)



--for enemy mage


{-| This function will return List of Positions that can be hit by Mage on the Board.
-}
attackedByMageRange : Pos -> List Pos
attackedByMageRange pos =
    List.map (vecAdd pos) subsubneighbour


{-| This function will update the target's position according to the hero's position
-}
updateTarget : Board -> Board
updateTarget board =
    case selectedHero board.heroes of
        Nothing ->
            { board | target = [] }

        Just hero ->
            case findHexagon board.pointPos board.level of
                Just cell ->
                    if List.member cell (listUnion board.attackable board.skillable) then
                        case hero.class of
                            Mage ->
                                { board | target = cell :: List.map (vecAdd cell) neighbour }

                            _ ->
                                { board | target = [ cell ] }

                    else
                        { board | target = [] }

                Nothing ->
                    { board | target = [] }


{-| This function will detect the selected hero.
-}
selectedHero : List Hero -> Maybe Hero
selectedHero hero_list =
    List.head (List.filter (\hero -> hero.selected) hero_list)


{-| This function will detect the unselected hero.
-}
unselectedHero : List Hero -> List Hero
unselectedHero hero_list =
    List.filter (\hero -> not hero.selected) hero_list


{-| This function will give List of unmoveable Positions.
-}
unMoveable : Board -> List Pos
unMoveable board =
    List.map .pos board.obstacles ++ List.map .pos board.enemies ++ List.map .pos board.heroes


{-| This function will destroy the destructable obstacle if being hit.
-}
checkAttackObstacle : List Pos -> Board -> Board
checkAttackObstacle pos_list board =
    let
        ( attackedObstacles, others ) =
            List.partition (\obstacle -> List.member obstacle.pos pos_list) board.obstacles

        ( attackedBreakable, attackedOthers ) =
            List.partition (\obstacle -> obstacle.obstacleType == MysteryBox) attackedObstacles
    in
    { board | obstacles = attackedOthers ++ others, item = List.map (\obstacle -> Item obstacle.itemType obstacle.pos) attackedBreakable ++ board.item }


{-| This function will set the limit of the maximum turret on the board.
-}
maxTurret : Int
maxTurret =
    2


sampleTurret : Int -> Int -> Pos -> Board -> Hero
sampleTurret health dmg pos board =
    Hero Turret pos (health - 10) (health - 10) (2 * dmg) 0 False Waiting (board.totalHeroNumber + 1)


{-| This function will let Engineer build Turret.
-}
checkBuildTurret : Hero -> Pos -> Board -> Board
checkBuildTurret myhero pos board =
    case myhero.class of
        Engineer ->
            let
                newherolist =
                    if isGridEmpty pos board then
                        sampleTurret myhero.maxHealth myhero.damage pos board :: board.heroes

                    else
                        board.heroes

                otherHeroes =
                    List.filter (\hero -> hero.class /= Engineer) board.heroes
            in
            case pos2Hero board.heroes pos of
                Nothing ->
                    if (board.heroes |> List.filter (\x -> x.class == Turret) |> List.length) < maxTurret then
                        { board | heroes = newherolist, totalHeroNumber = board.totalHeroNumber + 1, boardState = HeroAttack }

                    else
                        { board | popUpHint = ( FailtoBuild, 0 ), heroes = myhero :: otherHeroes }

                Just hero ->
                    if hero.class == Turret then
                        { board | heroes = listDifference board.heroes [ hero ], boardState = HeroAttack }

                    else
                        board

        _ ->
            board


{-| This function will check if a character receive a heal.
-}
checkHeal : Class -> Pos -> Board -> Board
checkHeal class pos board =
    case selectedHero board.heroes of
        Nothing ->
            board

        Just myhealer ->
            case class of
                Healer ->
                    case pos2Hero board.heroes pos of
                        Nothing ->
                            board

                        Just hero ->
                            let
                                others =
                                    listDifference board.heroes [ hero ]

                                nhealth =
                                    (hero.health + calculateHeal myhealer.damage)
                                        |> min hero.maxHealth

                                healthDif =
                                    nhealth - hero.health

                                newlist =
                                    { hero | health = nhealth, state = GettingHealed healthDif } :: others
                            in
                            { board | heroes = newlist, boardState = Healing }

                _ ->
                    board


{-| This function will calculate the heal according to the Healer's damage.
-}
calculateHeal : Int -> Int
calculateHeal damage =
    2 * damage


{-| This function will detect if that position have item.
-}
pos2Item : List Item -> Pos -> Item
pos2Item all_items pos =
    case List.filter (\x -> pos == x.pos) all_items of
        [] ->
            Item NoItem ( 999, 999 )

        chosen :: _ ->
            chosen


pos2Hero : List Hero -> Pos -> Maybe Hero
pos2Hero all_hero pos =
    case List.filter (\x -> pos == x.pos) all_hero of
        [] ->
            Nothing

        chosen :: _ ->
            Just chosen


{-| This function will convert from the hero index to hero in the choosing hero scene.
-}
index2Hero : Int -> List Hero -> Hero
index2Hero index l_hero =
    case List.filter (\x -> index == x.indexOnBoard) l_hero of
        [] ->
            Hero Warrior ( 0, 0 ) 80 -1 15 3 False Waiting 0

        chosen :: _ ->
            chosen


isGridEmpty : Pos -> Board -> Bool
isGridEmpty pos board =
    not
        ((List.map .pos board.obstacles
            ++ List.map .pos board.item
            ++ List.map .pos board.enemies
            ++ List.map .pos board.heroes
         )
            |> List.member pos
        )


isEngineerSkillGrid : Pos -> Board -> Bool
isEngineerSkillGrid pos board =
    not
        ((List.map .pos board.obstacles
            ++ List.map .pos board.item
            ++ List.map .pos board.enemies
            ++ List.map .pos (List.filter (\x -> x.class /= Turret) board.heroes)
         )
            |> List.member pos
        )
