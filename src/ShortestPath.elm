module ShortestPath exposing (leastArcherPath, leastHealerPath, leastMagePath, leastWarriorPath)

{-| This file fills functions that finds Shortest Path for each class.


# Functions

@docs leastArcherPath, leastHealerPath, leastMagePath, leastWarriorPath

-}

import Action exposing (attackedByArcherRange, attackedByMageRange)
import List exposing (append, minimum, partition)
import ListOperation exposing (listDifference, unionList)
import Type exposing (Board, Enemy, Pos, Spa_row)
import VectorOperation exposing (distance, neighbour, vecAdd)


{-| Output the warrior and assassin shortest path towards the nearest hero. The output path does not include the begin Pos.

`my_enemy` is the enemy that is to move

`board` is the current board

-}
leastWarriorPath : Enemy -> Board -> List Pos
leastWarriorPath my_enemy board =
    List.map getNeighbour board.heroes
        |> unionList
        |> leastPathHelper my_enemy board


getNeighbour : { a | pos : Pos } -> List Pos
getNeighbour tgt =
    List.map (vecAdd tgt.pos) neighbour


{-| Output the archer shortest path towards the nearest position to attack heroes. The output path does not include the begin Pos.

`my_enemy` is the enemy that is to move

`board` is the current board

-}
leastArcherPath : Enemy -> Board -> List Pos
leastArcherPath my_enemy board =
    List.map (attackedByArcherRange board) (List.map .pos board.heroes)
        |> unionList
        |> leastPathHelper my_enemy board


{-| Output the mage shortest path towards the nearest position to attack heroes. The output path does not include the begin Pos.
Remark : the output path does not include the begin Pos

`my_enemy` is the enemy that is to move

`board` is the current board

-}
leastMagePath : Enemy -> Board -> List Pos
leastMagePath my_enemy board =
    List.map attackedByMageRange (List.map .pos board.heroes)
        |> unionList
        |> leastPathHelper my_enemy board


{-| Output the healer shortest path towards the nearest position to heal other enemies. The output path does not include the begin Pos.

`my_enemy` is the enemy that is to move

`board` is the current board

-}
leastHealerPath : Enemy -> Board -> List Pos
leastHealerPath my_enemy board =
    listDifference board.enemies [ my_enemy ]
        |> List.map getNeighbour
        |> unionList
        |> leastPathHelper my_enemy board


{-| A useful helper to find the shortest path from my\_enemy's position towards a list of end positions.

`my_enemy` is the enemy that is to move

`board` is the current board

-}
leastPathHelper : Enemy -> Board -> List Pos -> List Pos
leastPathHelper my_enemy board tgt_list =
    let
        enemy_list =
            board.enemies

        hero_list =
            board.heroes

        barrier_list =
            board.obstacles

        whole_map =
            board.map

        unmoveable =
            List.map .pos enemy_list
                ++ List.map .pos hero_list
                ++ List.map .pos barrier_list
    in
    if List.member my_enemy.pos tgt_list then
        []

    else
        case tgt_list of
            [] ->
                []

            _ ->
                shortestPath whole_map unmoveable my_enemy.pos tgt_list


{-| Output the shortest path in the form of (List Pos).
Remark : the output path does not include the begin Pos

`wholemap` is the map of the board

`unmoveable` is a list of the positions can be reached

`target` is a list of target positions

`begin` is the Pos of the character i.e. one enemy or hero

`end_list` is the target Pos

For example, in the simplest situation, where there is no hero, enemy or obstacle,
shortestPath board hero\_list (1,5) (4,5) == [(2,5), (3,5), (4,5)]

-}
shortestPath : List Pos -> List Pos -> Pos -> List Pos -> List Pos
shortestPath wholemap unmoveable begin end_list =
    let
        ( _, path ) =
            shortPathFind unmoveable begin end_list ( ( initVisited, initTable begin wholemap ), [] )
    in
    path


shortPathFind : List Pos -> Pos -> List Pos -> ( ( List Pos, List Spa_row ), List Pos ) -> ( ( List Pos, List Spa_row ), List Pos )
shortPathFind unmoveable begin end_list ( ( visited, table ), path ) =
    let
        chosen =
            --the optimal one node
            case List.head visited of
                Just ps ->
                    ps

                Nothing ->
                    begin

        ( n_table, n_visited ) =
            updateTable unmoveable visited chosen table

        found_list =
            List.filter (\x -> isFindEndList end_list x) table
                |> List.map .pos
    in
    if not (List.isEmpty found_list) then
        case found_list of
            [] ->
                ( ( visited, table ), [] )

            end :: _ ->
                ( ( visited, table )
                , sortPath begin end table [ end ]
                )

    else if n_visited == visited then
        ( ( visited, table ), [] )

    else
        shortPathFind unmoveable begin end_list ( ( n_visited, n_table ), [] )


isFindEndList : List Pos -> Spa_row -> Bool
isFindEndList end row =
    if List.member row.pos end then
        case row.pre_pos of
            Just _ ->
                True

            Nothing ->
                False

    else
        False


isMyPos : Pos -> Spa_row -> Bool
isMyPos target row =
    if target == row.pos then
        True

    else
        False


getPrePos : Pos -> List Spa_row -> Pos
getPrePos pos table =
    case (pos2Spa_row pos table).pre_pos of
        Just pre ->
            pre

        Nothing ->
            ( 999, 999 )


pos2Spa_row : Pos -> List Spa_row -> Spa_row
pos2Spa_row pos table =
    let
        target_row =
            List.head (List.filter (isMyPos pos) table)
    in
    case target_row of
        Just row ->
            row

        Nothing ->
            { pos = ( 999, 999 ), pre_pos = Nothing, path_length = 999 }


sortPath : Pos -> Pos -> List Spa_row -> List Pos -> List Pos
sortPath begin end table sorted =
    let
        pre_position =
            getPrePos end table
    in
    if pre_position == begin then
        sorted

    else
        sortPath begin pre_position table (pre_position :: sorted)


initTable : Pos -> List Pos -> List Spa_row
initTable begin list_pos =
    List.map (\x -> { pos = x, path_length = 999, pre_pos = Nothing }) list_pos
        |> updateSpa_row { pos = begin, path_length = 0, pre_pos = Nothing }


initVisited : List Pos
initVisited =
    []


isAdjacent : List Pos -> List Pos -> Pos -> Spa_row -> Bool
isAdjacent unmoveable visited my_pos row =
    if
        (distance my_pos row.pos == 1)
            && not (List.member row.pos (unmoveable ++ visited))
    then
        True

    else
        False


updateTable : List Pos -> List Pos -> Pos -> List Spa_row -> ( List Spa_row, List Pos )
updateTable unmoveable visited pos table =
    let
        chosen =
            pos2Spa_row pos table

        ( adjacent, others ) =
            partition (isAdjacent unmoveable visited pos) table

        n_table =
            List.map (updateAdjacent chosen) adjacent ++ others
    in
    case chooseChosen n_table visited of
        ( 999, 999 ) ->
            ( n_table, visited )

        _ ->
            ( n_table, chooseChosen n_table visited :: visited )


updateAdjacent : Spa_row -> Spa_row -> Spa_row
updateAdjacent chosen adjacent =
    if chosen.path_length + 1 > adjacent.path_length then
        adjacent

    else
        { adjacent | path_length = chosen.path_length + 1, pre_pos = Just chosen.pos }


updateSpa_row : Spa_row -> List Spa_row -> List Spa_row
updateSpa_row n_row table =
    let
        this_pos =
            n_row.pos
    in
    partition (isMyPos this_pos) table
        |> Tuple.second
        |> append [ n_row ]


chooseChosen : List Spa_row -> List Pos -> Pos
chooseChosen table visited =
    let
        least =
            List.partition (isVisited visited) table
                |> Tuple.second
                |> List.map (\x -> x.path_length)
                |> minimum

        min_path =
            case least of
                Just lst ->
                    lst

                Nothing ->
                    -1
    in
    (pathLength2SpaRow min_path table visited).pos


isVisited : List Pos -> Spa_row -> Bool
isVisited visited row =
    List.any (\x -> x == row.pos) visited


pathLength2SpaRow : Int -> List Spa_row -> List Pos -> Spa_row
pathLength2SpaRow leng table visited =
    let
        possible_list =
            List.filter (isPossibleLengthRow leng visited) table
                |> List.head
    in
    case possible_list of
        Just row ->
            row

        Nothing ->
            { pos = ( 999, 999 ), pre_pos = Nothing, path_length = 999 }



--999 for not existing


isPossibleLengthRow : Int -> List Pos -> Spa_row -> Bool
isPossibleLengthRow leng visited row =
    row.path_length == leng && not (isVisited visited row)
