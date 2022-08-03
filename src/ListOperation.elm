module ListOperation exposing (cartesianProduct, intersectionList, listDifference, listIntersection, listUnion, unionList)

{-| This file fills functions related to wide used list operations.


# Functions

@docs cartesianProduct, intersectionList, listDifference, listIntersection, listUnion, unionList

-}


{-| This function will return the intersection between two lists with the same element type
-}
listIntersection : List a -> List a -> List a
listIntersection list1 list2 =
    List.filter (\x -> List.member x list2) list1


{-| This function will return the intersection of all lists in the nested `List (List a)`
-}
intersectionList : List (List a) -> List a
intersectionList llist =
    case llist of
        [] ->
            []

        [ list ] ->
            list

        list1 :: (list2 :: rest) ->
            intersectionList (listIntersection list1 list2 :: rest)


{-| This function will return every element in `list1` that is not in `list2`
-}
listDifference : List a -> List a -> List a
listDifference list1 list2 =
    List.filter (\x -> not (List.member x list2)) list1


{-| This functnion will return the union of two lists with same element type
-}
listUnion : List a -> List a -> List a
listUnion list1 list2 =
    let
        newElements =
            List.filter (\x -> not (List.member x list2)) list1
    in
    list2 ++ newElements


{-| This function will return the list of union of all lists in the nested `List (List a)`
-}
unionList : List (List a) -> List a
unionList list_of_list =
    case list_of_list of
        [] ->
            []

        [ list ] ->
            list

        list1 :: (list2 :: restlists) ->
            unionList (listUnion list1 list2 :: restlists)


{-| This function will apply function `f` to every pair possible between `List a` and `List b` and return `List c`.
-}
cartesianProduct : (a -> b -> c) -> List a -> List b -> List c
cartesianProduct f x y =
    List.concatMap (\x_ -> List.map (f x_) y) x
