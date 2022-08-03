module ViewConst exposing (halfWid, logoHeight, logoWidth, pixelHeight, pixelWidth, sideLen)

{-| This file fills functions related to the constant of the screen, each hexagon cell, and the logo.


# Function

@docs halfWid, logoHeight, logoWidth, pixelHeight, pixelWidth, sideLen

-}


{-| This function will return the constant width of the screen.
-}
pixelWidth : Float
pixelWidth =
    2000


{-| This function will return the constant height of the screen.
-}
pixelHeight : Float
pixelHeight =
    1000


{-| This function will return the constant length of the hexagon cell.
-}
sideLen : Float
sideLen =
    70


{-| This function will return the constant half-width of the hexagon cell.
-}
halfWid : Float
halfWid =
    35 * sqrt 3


{-| This function will return the constant logo width.
-}
logoWidth : Float
logoWidth =
    300 * sqrt 3


{-| This function will return the constant logo height.
-}
logoHeight : Float
logoHeight =
    600
