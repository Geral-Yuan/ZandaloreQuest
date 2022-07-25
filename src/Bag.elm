module Bag exposing (Bag, addCoin, initBag)


type alias Bag =
    { coins : Int
    }


initBag : Bag
initBag =
    { coins = 0
    }


addCoin : Bag -> Int -> Bag
addCoin bag num =
    { bag | coins = bag.coins + num }
