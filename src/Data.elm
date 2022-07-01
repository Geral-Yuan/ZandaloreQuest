module Data exposing (..)

import Svg.Attributes exposing (x2, y2)


pixelWidth : Float
pixelWidth =
    1200


pixelHeight : Float
pixelHeight =
    1000


sideLen : Float
sideLen =
    70


halfWid : Float
halfWid =
    35 * sqrt (toFloat 3)


posToString : ( Float, Float ) -> String
posToString ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


vecAdd : Pos -> Pos -> Pos
vecAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


vecScale : Int -> Pos -> Pos
vecScale a ( x, y ) =
    ( a * x, a * y )


cartesianProduct : (a -> b -> c) -> List a -> List b -> List c
cartesianProduct f x y =
    List.concatMap (\x_ -> List.map (f x_) y) x


findPos : ( Int, Int ) -> ( Float, Float )
findPos ( row, column ) =
    ( 600 + toFloat (row - column) * halfWid, toFloat (80 + (row + column - 6) * 105) )


type alias Pos =
    ( Int, Int )


type Orientation
    = LeftUp
    | RightUp
    | Right
    | RightDown
    | LeftDown
    | Left


type Turn
    = HeroTurn
    | EnemyTurn


type Class
    = Warrior
    | Archer
    | Assassin
    | Mage


type alias Hero =
    { class : Class
    , pos : Pos
    , health : Int
    , damage : Int
    , armour : Int
    , energy : Int
    , selected : Bool
    , indexOnBoard : Int --give an index to the heroes on the board
    }


type alias Enemy =
    { class : Class
    , pos : Pos
    , health : Int
    , damage : Int
    , armour : Int
    , steps : Int
    , done : Bool
    , indexOnBoard : Int --give an index to the enemies on the board
    }


type Dir
    = W
    | E
    | D
    | X
    | Z
    | A


distance : Pos -> Pos -> Int
distance ( x1, y1 ) ( x2, y2 ) =
    let
        maxDis =
            max (max (abs (x1 - x2)) (abs (y1 - y2))) (abs (x1 + y1 - x2 - y2))
    in
    abs (x1 - x2) + abs (y1 - y2) + abs (x1 + y1 - x2 - y2) - maxDis


leastdistance : List Pos -> Pos -> Maybe Int
leastdistance pos_list pos =
    List.minimum (List.map (distance pos) pos_list)


detOrientation : Pos -> Pos -> Orientation
detOrientation ( x1, y1 ) ( x2, y2 ) =
    let
        orient1 =
            y1 >= y2

        orient2 =
            x1 <= x2

        orient3 =
            x1 + y1 <= x2 + y2
    in
    case ( orient1, orient2 ) of
        ( True, False ) ->
            RightDown

        ( True, True ) ->
            if not orient3 then
                LeftDown

            else
                Left

        ( False, True ) ->
            RightUp

        ( False, False ) ->
            Right


isWarriorAttackRange : Pos -> Pos -> Bool
isWarriorAttackRange attacked me =
    let
        ( x, y ) =
            attacked
    in
    if
        me
            == ( x + 1, y )
            || me
            == ( x, y + 1 )
            || me
            == ( x + 1, y - 1 )
            || me
            == ( x, y - 1 )
            || me
            == ( x - 1, y )
            || me
            == ( x - 1, y + 1 )
    then
        True

    else
        False
