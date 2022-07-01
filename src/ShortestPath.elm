module ShortestPath exposing (leastPath)

import Board exposing (Board)
import Data exposing (Hero, Enemy, Pos, distance)
import List exposing (append, minimum, partition)


type alias Spa_row =
    { pos : Pos
    , path_length : Int
    , pre_pos : Maybe Pos
    }


{- Output the shortest path towards the nearest hero in the form of (List Pos).
   Remark : the output path does not include the begin Pos

   "my_enemy" is the enemy that is to move

   "board" is the current board

   "hero_list" is the current List of Hero, which includes all heroes

-}

leastPath : Enemy -> Board -> List Hero -> List Pos
leastPath my_enemy board list_hero =
    leastPathHelper my_enemy board list_hero (getHeroesAdjacent board list_hero)

getHeroesAdjacent :  Board -> List Hero -> List Pos
getHeroesAdjacent board list_hero =
    List.map (\x -> List.filter (checkAdjacent board list_hero x.pos ) board.map) list_hero
    |> List.concat
    


leastPathHelper : Enemy -> Board -> List Hero -> List Pos -> List Pos
leastPathHelper my_enemy board list_hero tgt_list =
    if List.any (\x -> (distance my_enemy.pos x.pos == 1)) list_hero then
        []
    else
    case tgt_list of
        [] ->
            []
        [one_pos] ->
            shortestPath board list_hero my_enemy.pos one_pos
        hero1 :: hero2 :: r_lst ->
            let
                length1 = shortestPathLength board list_hero my_enemy.pos hero1
                length2 = shortestPathLength board list_hero my_enemy.pos hero2
            in
            
            if ((length1 < length2) && (length1 > 0)) then
                leastPathHelper my_enemy board list_hero (hero1 :: r_lst)
            else if ((length2 <= length1) && (length2 > 0)) then
                leastPathHelper my_enemy board list_hero (hero2 :: r_lst)
            else
                []


shortestPathLength : Board -> List Hero -> Pos -> Pos -> Int
shortestPathLength board hero_list begin end =
    List.length (shortestPath board hero_list begin end)


{- Output the shortest path in the form of (List Pos).
   Remark : the output path does not include the begin Pos

   "board" is the current board

   "hero_list" is the current List of Hero, which includes all heroes

   "begin" is the Pos of the character i.e. one enemy or hero

   "end" is the target Pos

   For example, in the simplest situation, where there is no hero, enemy or obstacle,
   shortestPath board hero_list (1,5) (4,5) == [(2,5), (3,5), (4,5)]

-}


shortestPath : Board -> List Hero -> Pos -> Pos -> List Pos
shortestPath board hero_list begin end =
    shortPathFind board hero_list begin end ( ( initVisited, initTable begin board.map ), [] )
        |> Tuple.second


shortPathFind : Board -> List Hero -> Pos -> Pos -> ( ( List Pos, List Spa_row ), List Pos ) -> ( ( List Pos, List Spa_row ), List Pos )
shortPathFind board hero_list begin end ( ( visited, table ), path ) =
    let
        chosen =
            --the optimal one node
            case List.head visited of
                Just ps ->
                    ps

                Nothing ->
                    begin

        ( n_table, n_visited ) =
            updateTable board hero_list visited chosen table
    in
    if List.any (isFindEnd end) table then
        ( ( visited, table )
        , sortPath begin end table [ end ]
        )

    else if n_visited == visited then
        ( ( visited, table ), [] )

    else
        shortPathFind board hero_list begin end ( ( n_visited, n_table ), [] )


isFindEnd : Pos -> Spa_row -> Bool
isFindEnd end row =
    if end == row.pos then
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


checkAdjacent : Board -> List Hero -> Pos -> Pos -> Bool
checkAdjacent board hero_list  my_pos row =
    if
        (distance my_pos row == 1)
            && not
                (List.member row
                    (board.barrier
                        ++ List.map .pos hero_list
                        ++ List.map .pos board.enemies
                    )
                )
    then
        True

    else
        False

isAdjacent : Board -> List Hero -> List Pos -> Pos -> Spa_row -> Bool
isAdjacent board hero_list visited my_pos row =
    if
        (distance my_pos row.pos == 1)
            && not
                (List.member row.pos
                    (board.barrier
                        ++ List.map .pos hero_list
                        ++ List.map .pos board.enemies
                        ++ visited
                    )
                )
    then
        True

    else
        False


updateTable : Board -> List Hero -> List Pos -> Pos -> List Spa_row -> ( List Spa_row, List Pos )
updateTable board hero_list visited pos table =
    let
        chosen =
            pos2Spa_row pos table

        ( adjacent, others ) =
            partition (isAdjacent board hero_list visited pos) table

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
    (pathLength2Spa_row min_path table visited).pos


isVisited : List Pos -> Spa_row -> Bool
isVisited visited row =
    if List.any (\x -> x == row.pos) visited then
        True

    else
        False


pathLength2Spa_row : Int -> List Spa_row -> List Pos -> Spa_row
pathLength2Spa_row leng table visited =
    let
        possible_list =
            List.filter (\x -> x.path_length == leng) (Tuple.second (List.partition (isVisited visited) table)) |> List.head
    in
    case possible_list of
        Just row ->
            row

        Nothing ->
            { pos = ( 999, 999 ), pre_pos = Nothing, path_length = 999 }
