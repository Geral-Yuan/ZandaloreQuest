module Bag exposing (addCoin, initBag)

import Type exposing (Bag)


initBag : Bag
initBag =
    { coins = 0
    }


addCoin : Bag -> Int -> Bag
addCoin bag num =
    { bag | coins = bag.coins + num }
