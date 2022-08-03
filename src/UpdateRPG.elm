module UpdateRPG exposing (updateRPG)

{-| This file fills functions related to updating RPG game mode.


# Function

@docs updateRPG

-}

import Data exposing (allSampleHeroes)
import Message exposing (Msg(..))
import NPC exposing (allNPC)
import Type exposing (Class(..), Dir(..), GameMode(..), Model, Task(..))
import UpdateScene exposing (checkLeaveCastle, checkLeaveDungeon, checkLeaveDungeon2, checkLeaveShop)
import ViewNPCTask exposing (checkTalkRange)


{-| This function update RPG mode
-}
updateRPG : Msg -> Model -> ( Model, Cmd Msg )
updateRPG msg model =
    let
        character =
            model.character

        ( x, y ) =
            character.pos
    in
    case msg of
        Enter False ->
            case model.mode of
                Shop ->
                    ( model |> checkLeaveShop, Cmd.none )

                Castle ->
                    ( model |> checkLeaveCastle, Cmd.none )

                Dungeon ->
                    ( model |> checkLeaveDungeon, Cmd.none )

                Dungeon2 ->
                    ( model |> checkLeaveDungeon2, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Talk False ->
            if model.mode == Shop then
                if x > 600 && x < 1000 && y <= 650 then
                    ( { model | mode = BuyingItems }, Cmd.none )

                else
                    ( model, Cmd.none )

            else
                ( model |> checkTalkRange, Cmd.none )

        Key Left on ->
            ( { model | character = { character | moveLeft = on, moveRight = character.moveRight && not on } }, Cmd.none )

        Key Right on ->
            ( { model | character = { character | moveRight = on, moveLeft = character.moveLeft && not on } }, Cmd.none )

        Key Up on ->
            ( { model | character = { character | moveUp = on, moveDown = character.moveDown && not on } }, Cmd.none )

        Key Down on ->
            ( { model | character = { character | moveDown = on, moveUp = character.moveUp && not on } }, Cmd.none )

        Test ->
            ( { model
                | test = True
                , npclist = allNPC
                , unlockShop = True
                , unlockDungeon = True
                , unlockDungeon2 = True
                , cntTask = BeatBoss
                , indexedheroes = allSampleHeroes
              }
            , Cmd.none
            )

        SeeEncyclopedia ->
            ( { model | previousMode = model.mode, mode = Encyclopedia Warrior }, Cmd.none )

        _ ->
            ( model, Cmd.none )
