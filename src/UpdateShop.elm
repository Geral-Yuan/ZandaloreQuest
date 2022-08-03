module UpdateShop exposing (updateShop)

{-| This file fills functions related to update in the shop.


# Functions

@docs updateShop

-}

import Data exposing (allSampleHeroes, upgradeDamage, upgradeHealth)
import ListOperation exposing (listDifference)
import Message exposing (Msg(..))
import NPC exposing (npcMap)
import Random exposing (generate, Generator, uniform)
import Type exposing (Class(..), Dir(..), GameMode(..), Hero, Model, Task(..))


{-| "updateShop" includes entering the BuyItem page, lucky draw, entering upgrade page, switching upgraded heroes, upgrading and exit.
-}
updateShop : Msg -> Model -> ( Model, Cmd Msg )
updateShop msg model =
    let
        currCoins =
            model.bag.coins

        newBag =
            model.bag
    in
    case msg of
        LuckyDraw ->
            if model.cntTask == GoToShop then
                ( { model | cntTask = Level 1, npclist = npcMap 8 :: model.npclist }, generate GetNewHero (drawHero model) )

            else if model.bag.coins > 99 then
                ( { model | bag = { newBag | coins = currCoins - 100 } }, generate GetNewHero (drawHero model) )

            else
                ( model, Cmd.none )

        GetNewHero newclass ->
            let
                newhero =
                    case newclass of
                        Turret ->
                            model.indexedheroes

                        _ ->
                            List.filter (\( hero, _ ) -> hero.class == newclass) allSampleHeroes
            in
            ( { model | indexedheroes = newhero ++ model.indexedheroes, mode = DrawHero newclass }, Cmd.none )

        EnterUpgrade ->
            ( { model | mode = UpgradePage }, Cmd.none )

        LevelUp hero ->
            if model.bag.coins > 49 then
                ( { model
                    | bag = { newBag | coins = currCoins - 50 }
                    , indexedheroes = (hero |> updateDamage |> updateHealth) :: listDifference model.indexedheroes [ hero ]
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ExitShop ->
            case model.mode of
                UpgradePage ->
                    ( { model | mode = BuyingItems }, Cmd.none )

                DrawHero _ ->
                    ( { model | mode = BuyingItems }, Cmd.none )

                _ ->
                    ( { model | mode = Shop }, Cmd.none )

        Key Left False ->
            if model.mode == UpgradePage then
                ( { model | upgradePageIndex = modBy 6 (model.upgradePageIndex - 2) + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        Key Right False ->
            if model.mode == UpgradePage then
                ( { model | upgradePageIndex = modBy 6 model.upgradePageIndex + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        DisplayUpgrade on ->
            ( { model | isDisplayUpgrade = on }, Cmd.none )

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

        adhealth =
            upgradeHealth currHero.class
    in
    ( { currHero | health = currHealth + adhealth, maxHealth = currHealth + adhealth }, index )


updateDamage : ( Hero, Int ) -> ( Hero, Int )
updateDamage hero =
    let
        currHero =
            Tuple.first hero

        index =
            Tuple.second hero

        currDamage =
            currHero.damage

        adddmg =
            upgradeDamage currHero.class
    in
    ( { currHero | damage = currDamage + adddmg }, index )


drawHero : Model -> Generator Class
drawHero model =
    let
        have_class =
            List.map (\( hero, _ ) -> hero.class) model.indexedheroes

        nothave =
            listDifference [ Warrior, Archer, Mage, Assassin, Healer, Engineer ] have_class
    in
    case nothave of
        [] ->
            uniform Turret []

        class :: others ->
            uniform class others
