module Bag exposing (addCoin, initBag)

{-| This file fills functions related to bag and coins.


# Functions

@docs addCoin, initBag

-}

import Type exposing (Bag)


{-| This function will initiate bag with zero coins.
-}
initBag : Bag
initBag =
    { coins = 0
    }


{-| This function will add coins to the bag.
-}
addCoin : Bag -> Int -> Bag
addCoin bag num =
    { bag | coins = bag.coins + num }
