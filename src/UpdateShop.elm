module UpdateShop exposing (updateShop)

import Data exposing (..)
import Message exposing (Msg(..))
import Model exposing (Model)
--import Random exposing (Generator)

updateShop : Msg -> Model -> ( Model, Cmd Msg )
updateShop msg model =
    let

        currCoins =
            model.bag.coins

        newBag =
            model.bag

        currHeroes =
            model.indexedheroes
    in
    case msg of
        UpgradeHealth ->
            if model.bag.coins > 49 then
                ( { model | bag = { newBag | coins = currCoins - 50 }, indexedheroes = List.map updateHealth currHeroes }, Cmd.none )

            else
                ( model, Cmd.none )

        UpgradeDamage ->
            if model.bag.coins > 49 then
                ( { model | bag = { newBag | coins = currCoins - 50 }, indexedheroes = List.map updateDamage currHeroes }, Cmd.none )

            else
                ( model, Cmd.none )

        ExitShop ->
            ( { model | mode = Shop }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateHealth : ( Hero, Int ) -> ( Hero, Int )
updateHealth hero =
    let
        currHero =
            Tuple.first hero

        index =
            Tuple.second hero

        currHealth =
            currHero.health
    in
    ( { currHero | health = currHealth + 5, maxHealth = currHealth + 5 }, index )


updateDamage : ( Hero, Int ) -> ( Hero, Int )
updateDamage hero =
    let
        currHero =
            Tuple.first hero

        index =
            Tuple.second hero

        currDamage =
            currHero.damage
    in
    ( { currHero | damage = currDamage + 2 }, index )