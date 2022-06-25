module Data exposing (..)


pixelWidth : Float
pixelWidth =
    1000


pixelHeight : Float
pixelHeight =
    1000

sideLen : Float
sideLen =
    70

halfWid : Float
halfWid =
    35 * sqrt (toFloat 3)


posToString : (Float, Float) -> String
posToString (x,y) =
    String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


type alias Pos =
    ( Int, Int )


type Class
    = Warrior
    | Assassin
    | Mage
    | Archer


type alias Character =
    { class : Class
    , pos : Pos
    , health : Int
    , damage : Int
    , armour : Int
    }


type alias Enemy =
    { pos : Pos
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
