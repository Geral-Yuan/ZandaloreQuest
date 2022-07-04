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

                can_attack =
                    listintersection realattackRange (List.map .pos board.obstacles ++ List.map .pos board.enemies)
            in
            { board | attackable = can_attack }


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
            listintersection linePos (List.map .pos board.obstacles ++ List.map .pos board.enemies)
    in
    case leastdistance inWay heropos of
        Nothing ->
            sameline pos

        Just dis ->
            List.map (\k -> vecScale k pos) (List.range 1 dis)


updateMoveable : Board -> Board
updateMoveable board =
    case selectedHero board.heroes of
        Nothing ->
            { board | moveable = [] }

        Just hero ->
            let
                realmoveRange =
                    List.map (vecAdd hero.pos) neighbour

                can_move =
                    List.filter (\pos -> not (List.member pos (List.map .pos board.obstacles ++ List.map .pos board.enemies ++ List.map .pos board.heroes))) realmoveRange
            in
            { board | moveable = can_move }

selectedHero : List Hero -> Maybe Hero
selectedHero hero_list =
    List.head (List.filter (\hero -> hero.selected) hero_list)


unselectedHero : List Hero -> List Hero
unselectedHero hero_list =
    List.filter (\hero -> not hero.selected) hero_list

checkObstacleType : Pos -> Obstacle -> ObstacleType
checkObstacleType ( row, column ) obstacle =
    if obstacle.pos == ( row, column ) then
        obstacle.obstacleType

    else
        NoObstacle


checkItemType : Pos -> Item -> ItemType
checkItemType ( row, column ) item =
    if item.pos == ( row, column ) then
        item.itemType

    else
        NoItem
