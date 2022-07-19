module Action exposing (..)

import Board exposing (Board)
import Data exposing (..)
import Message exposing (Msg(..))
import Time exposing (Weekday(..))


updateEnemyAttackable : Board -> Board
updateEnemyAttackable board =
    let
        maybeEnemy =
            List.head (List.filter (\x -> x.indexOnBoard == board.cntEnemy) board.enemies)
    in
    case maybeEnemy of
        Nothing ->
            { board | enemyAttackable = [] }

        Just enemy ->
            let
                realattackRange =
                    List.map (vecAdd enemy.pos) (attackRangeEnemy board enemy)
            in
            { board | enemyAttackable = realattackRange }


updateAttackable : Board -> Board
updateAttackable board =
    case selectedHero board.heroes of
        Nothing ->
            { board | attackable = [], skillable = [] }

        Just hero ->
            let
                realattackRange =
                    List.map (vecAdd hero.pos) (attackRange board hero)

                realskillRange =
                    case hero.class of
                        Healer ->
                            List.map (vecAdd hero.pos) (skillRange hero) |> listIntersection (List.map .pos board.heroes)

                        Engineer ->
                            List.map (vecAdd hero.pos) (skillRange hero) |> List.filter (\x -> isGridEmpty x board)

                        _ ->
                            []
            in
            { board | attackable = realattackRange, skillable = realskillRange }


attackRange : Board -> Hero -> List Pos
attackRange board hero =
    case hero.class of
        Archer ->
            List.concat (List.map (stuckInWay board hero.pos Friend) neighbour)

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


attackedByArcherRange : Board -> Pos -> List Pos
attackedByArcherRange board pos =
    List.map (vecAdd pos) (List.concat (List.map (stuckInWay board pos Hostile) neighbour))


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


attackedByMageRange : Pos -> List Pos
attackedByMageRange pos =
    pos :: List.map (vecAdd pos) subsubneighbour


updateMoveable : Board -> Board
updateMoveable board =
    case selectedHero board.heroes of
        Nothing ->
            { board | moveable = [] }

        Just hero ->
            let
                can_move =
                    List.map (\neighberpos -> ( vecAdd hero.pos neighberpos, neighborToDir neighberpos )) neighbour

                really_can_move =
                    List.filter (\moveable -> List.member (Tuple.first moveable) board.map && not (List.member (Tuple.first moveable) (unMoveable board))) can_move
            in
            { board | moveable = really_can_move }


updateTarget : Board -> Board
updateTarget board =
    case selectedHero board.heroes of
        Nothing ->
            { board | target = [] }

        Just hero ->
            case findHexagon board.pointPos of
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


selectedHero : List Hero -> Maybe Hero
selectedHero hero_list =
    List.head (List.filter (\hero -> hero.selected) hero_list)


unselectedHero : List Hero -> List Hero
unselectedHero hero_list =
    List.filter (\hero -> not hero.selected) hero_list


checkItemType : Pos -> Item -> ItemType
checkItemType ( row, column ) item =
    if item.pos == ( row, column ) then
        item.itemType

    else
        NoItem


unMoveable : Board -> List Pos
unMoveable board =
    List.map .pos board.obstacles ++ List.map .pos board.enemies ++ List.map .pos board.heroes


checkAttackObstacle : List Pos -> Board -> Board
checkAttackObstacle pos_list board =
    let
        ( attackedObstacles, others ) =
            List.partition (\obstacle -> List.member obstacle.pos pos_list) board.obstacles

        ( attackedBreakable, attackedOthers ) =
            List.partition (\obstacle -> obstacle.obstacleType == MysteryBox) attackedObstacles
    in
    { board | obstacles = attackedOthers ++ others, item = List.map (\obstacle -> Item obstacle.itemType obstacle.pos) attackedBreakable ++ board.item }


checkBuildObstacle : Class -> Pos -> Board -> Board
checkBuildObstacle class pos board =
    case class of
        Engineer ->
            let
                newobslist =
                    if isGridEmpty pos board then
                        Obstacle MysteryBox pos NoItem :: board.obstacles

                    else
                        board.obstacles
            in
            { board | obstacles = newobslist }

        _ ->
            board


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


calculateHeal : Int -> Int
calculateHeal damage =
    2 * damage


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
