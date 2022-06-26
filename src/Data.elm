module Data exposing (..)

import Time exposing (Posix)


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


findPos : ( Int, Int ) -> ( Float, Float )
findPos ( row, column ) =
    ( 600 + toFloat (row - column) * halfWid, toFloat (80 + (row + column - 6) * 105) )


type alias Pos =
    ( Int, Int )


type Class
    = Warrior
    | Archer
    | Assassin
    | Mage


type alias Character =
    { class : Class
    , pos : Pos
    , health : Int
    , damage : Int
    , armour : Int
    , selected : Bool
    }


type alias Enemy =
    { class : Class
    , pos : Pos
    , health : Int
    , damage : Int
    , armour : Int
    }


type Dir
    = W
    | E
    | D
    | X
    | Z
    | A
