module Action exposing (..)

import Board exposing (Board)
import Data exposing (..)
import Message exposing (Msg(..))


updateAttackable : Board -> Board
updateAttackable board =
    case selectedHero board.heroes of
        Nothing ->
            { board | attackable = [] }

        Just hero ->
            let
                realattackRange =
                    List.map (vecAdd hero.pos) (attackRange board hero)
            in
            { board | attackable = realattackRange }


attackRange : Board -> Hero -> List Pos
attackRange board hero =
    case hero.class of
        Archer ->
            List.concat (List.map (stuckInWay board hero.pos) neighbour)

        Mage ->
            subneighbour

        _ ->
            neighbour


attackedByArcherRange : Board -> Pos -> List Pos
attackedByArcherRange board pos =
    List.map (vecAdd pos) (List.concat (List.map (stuckInWay board pos) neighbour))


stuckInWay : Board -> Pos -> Pos -> List Pos
stuckInWay board heropos pos =
    let
        linePos =
            List.map (vecAdd heropos) (sameline pos)

        inWay =
            listIntersection linePos (List.map .pos board.obstacles ++ List.map .pos board.enemies)
    in
    case leastdistance inWay heropos of
        Nothing ->
            sameline pos

        Just dis ->
            List.map (\k -> vecScale k pos) (List.range 1 dis)



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
                    if List.member cell board.attackable then
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


pos2Item : List Item -> Pos -> Item
pos2Item all_items pos =
    case List.filter (\x -> (pos == x.pos)) all_items of
        [] ->
            Item NoItem (999, 999)
        chosen :: _ ->
            chosen

pos2Hero : List Hero -> Pos -> Hero
pos2Hero all_hero pos =
    case List.filter (\x -> (pos == x.pos)) all_hero of
        [] ->
            Hero Warrior ( 0, 0 ) -1 15 5 3 False 0
        chosen :: _ ->
            chosen


