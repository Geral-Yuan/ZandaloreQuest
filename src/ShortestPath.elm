module ShortestPath exposing (leastWarriorPath, leastArcherPath, getHeroesLines, isBetweenEmpty)

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

leastWarriorPath : Enemy -> Board -> List Hero -> List Pos
leastWarriorPath my_enemy board list_hero =
    leastWarriorPathHelper my_enemy board list_hero (getHeroesAdjacent board list_hero)

leastArcherPath : Enemy -> Board  -> List Pos
leastArcherPath my_enemy board  =
    leastArcherPathHelper my_enemy board ((getHeroesLines board), [])
    |> Tuple.second


getHeroesAdjacent :  Board -> List Hero -> List Pos
getHeroesAdjacent board list_hero =
    List.map (\x -> List.filter (checkAdjacent board list_hero x.pos ) board.map) list_hero
    |> List.concat
    
getHeroesLines :  Board -> List Pos
getHeroesLines board =
    List.map (\enemy -> (List.map (\hero -> List.filter (isBetweenEmpty board enemy hero.pos ) board.map) board.heroes
                        |> List.concat)
            ) board.enemies
            |> List.concat
    

isBetweenEmpty :  Board -> Enemy -> Pos -> Pos -> Bool
isBetweenEmpty board my_enemy my_pos row =
    let 
        ( my_x, my_y ) = my_pos
        ( row_x, row_y ) = row
        enemy = List.filter (\x -> (x /= my_enemy )) board.enemies
    in
    if
        ((my_x == row_x )||(my_y == row_y )||((my_x + my_y) == (row_x + row_y)))
        && not (List.member row
                    (board.barrier
                        ++ List.map .pos board.heroes
                        ++ List.map .pos enemy
                    )
                )
    then
        if (my_x == row_x ) then
            let 
                small_y = min my_y row_y
                big_y = max my_y row_y
                pre_list =  List.map (\y -> ( my_x, y )) (List.range small_y big_y )
                possible_list = 
                    if List.length pre_list == 2 then
                        [ row ]
                    else
                        pre_list
                        |> List.take (len)
                        |> List.drop 1
                len = big_y - small_y
            in
                if List.any identity (List.map (\pos -> List.member pos (board.barrier
                        ++ List.map .pos board.heroes
                        --++ List.map .pos board.enemies
                    )) possible_list)
                then
                    False
                else
                    True
        else if (my_y == row_y ) then
            let 
                small_x = min my_x row_x
                big_x = max my_x row_x
                pre_list = List.map (\x -> ( x, my_y )) (List.range small_x big_x )
                possible_list = 
                    if List.length pre_list == 2 then
                        [ row ]
                    else
                        pre_list
                        |> List.take (len)
                        |> List.drop 1
                                
                len = big_x - small_x
            in
                if List.any identity (List.map (\pos -> List.member pos (board.barrier
                        ++ List.map .pos board.heroes
                        --++ List.map .pos board.enemies
                    )) possible_list)
                then
                    False
                else
                    True
        else if (my_x + my_y) == (row_x + row_y) then
            let 
                sum_xy = my_x + my_y
                small_x = min my_x row_x
                big_x = max my_x row_x
                pre_list = List.map (\x -> ( x, sum_xy - x )) (List.range small_x big_x )
                possible_list = 
                    if List.length pre_list == 2 then
                        [ row ]
                    else
                        pre_list
                        |> List.take (len)
                        |> List.drop 1
                len = big_x - small_x

                
            in
                    
                if List.any identity (List.map (\pos -> List.member pos (board.barrier
                        ++ List.map .pos board.heroes
                        --++ List.map .pos board.enemies
                    )) possible_list)
                then
                    False
                else
                    True
        else 
            False
    else
        False

leastWarriorPathHelper : Enemy -> Board -> List Hero -> List Pos -> List Pos
leastWarriorPathHelper my_enemy board list_hero tgt_list =
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
                leastWarriorPathHelper my_enemy board list_hero (hero1 :: r_lst)
            else if ((length2 <= length1) && (length2 > 0)) then
                leastWarriorPathHelper my_enemy board list_hero (hero2 :: r_lst)
            else
                []

leastArcherPathHelper : Enemy -> Board -> (List Pos, List Pos) -> (List Pos, List Pos)
leastArcherPathHelper my_enemy board (tgt_list, opt) =
    let
        list_hero = board.heroes
    in
    if List.member my_enemy.pos tgt_list then
        ([], [])
    else
        case tgt_list of
        [] ->
            ([], opt)
        
        one_pos :: r_lst ->
            let
                path1 = shortestPath board list_hero my_enemy.pos one_pos
                length1 = List.length path1
                length2 = List.length opt
            in
            
            if ((length1 < length2 || length2 == 0 ) && (length1 > 0)) then
                (leastArcherPathHelper my_enemy board  (r_lst, path1 ))
            else
                leastArcherPathHelper my_enemy board  (r_lst, opt)

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
