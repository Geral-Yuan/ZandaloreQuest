module UpdateShop exposing (updateShop)

import Data exposing (Class(..), Dir(..), GameMode(..), Hero, Task(..), allSampleHeroes, listDifference)
import Message exposing (Msg(..))
import Model exposing (Model)
import NPC exposing (npcDarkKnight1)
import Random


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

        LuckyDraw ->
            if model.cntTask == GoToShop then
                ( { model | cntTask = Level 1, npclist = npcDarkKnight1 :: model.npclist }, Random.generate GetNewHero (drawHero model) )

            else if model.bag.coins > 99 then
                ( { model | bag = { newBag | coins = currCoins - 100 } }, Random.generate GetNewHero (drawHero model) )

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
            case currHero.class of
                Warrior -> 20
                Archer -> 10
                Assassin -> 10
                Mage -> 10
                Healer -> 15
                Engineer -> 10
                Turret -> 0
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
            case currHero.class of
                Warrior -> 2
                Archer -> 4
                Assassin -> 5
                Mage -> 2
                Healer -> 2
                Engineer -> 2
                Turret -> 0
    in
    ( { currHero | damage = currDamage + adddmg }, index )


drawHero : Model -> Random.Generator Class
drawHero model =
    let
        ( _, nothave ) =
            List.partition (\x -> List.member x (List.map (\( hero, _ ) -> hero.class) model.indexedheroes)) [ Warrior, Archer, Mage, Assassin, Healer, Engineer ]
    in
    case nothave of
        [] ->
            Random.uniform Turret []

        class :: others ->
            Random.uniform class others
