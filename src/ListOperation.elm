module ListOperation exposing (..)

{- get the intersection of two lists -}


listIntersection : List a -> List a -> List a
listIntersection list1 list2 =
    List.filter (\x -> List.member x list2) list1



{- get the intersection of all lists which are elements of input list -}


intersectionList : List (List a) -> List a
intersectionList llist =
    case llist of
        [] ->
            []

        [ list ] ->
            list

        list1 :: (list2 :: rest) ->
            intersectionList (listIntersection list1 list2 :: rest)



{- get list of elements in list1 but not in list2 -}


listDifference : List a -> List a -> List a
listDifference list1 list2 =
    List.filter (\x -> not (List.member x list2)) list1



{- get the union of two lists -}


listUnion : List a -> List a -> List a
listUnion list1 list2 =
    let
        newElements =
            List.filter (\x -> not (List.member x list2)) list1
    in
    list2 ++ newElements



{- get the union of all lists which are elements of input list -}


unionList : List (List a) -> List a
unionList list_of_list =
    case list_of_list of
        [] ->
            []

        [ list ] ->
            list

        list1 :: (list2 :: restlists) ->
            unionList (listUnion list1 list2 :: restlists)



{- apply f to each element in x and y -}


cartesianProduct : (a -> b -> c) -> List a -> List b -> List c
cartesianProduct f x y =
    List.concatMap (\x_ -> List.map (f x_) y) x
