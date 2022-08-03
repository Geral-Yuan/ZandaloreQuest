module UpdateTutorial exposing (updateTutorial)

{-| This file fills functions related to updating tutorial.


# Function

@docs updateTutorial

-}

import Bag exposing (addCoin)
import Message exposing (Msg(..))
import Type exposing (BoardState(..), Class(..), GameMode(..), Model, Task(..), Turn(..))
import UpdateBoard exposing (updateBeaten, updateBoardGame)


followTutorial : Msg -> Int -> Bool
followTutorial msg k =
    case k of
        2 ->
            case msg of
                Select hero ->
                    hero.class == Warrior

                _ ->
                    False

        3 ->
            msg == Move ( 3, 7 )

        4 ->
            msg == Move ( 4, 6 )

        5 ->
            case msg of
                Select hero ->
                    hero.class == Archer

                _ ->
                    False

        6 ->
            msg == Hit ( 5, 4 )

        7 ->
            msg == EndTurn

        8 ->
            case msg of
                Select hero ->
                    hero.class == Warrior

                _ ->
                    False

        9 ->
            msg == Move ( 4, 5 )

        10 ->
            msg == Move ( 5, 4 )

        11 ->
            case msg of
                Select hero ->
                    hero.class == Archer

                _ ->
                    False

        12 ->
            msg == Hit ( 7, 2 )

        13 ->
            case msg of
                Click _ _ ->
                    True

                _ ->
                    False

        _ ->
            False


{-| This function will update the tutorial
-}
updateTutorial : Msg -> Int -> Model -> ( Model, Cmd Msg )
updateTutorial msg k model =
    case k of
        1 ->
            case msg of
                Click _ _ ->
                    ( { model | mode = Tutorial 2 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        14 ->
            ( { model | mode = BoardGame }, Cmd.none )

        _ ->
            case msg of
                Tick _ ->
                    updateBoardGame msg model

                Attack _ _ ->
                    updateBoardGame msg model

                Kill False ->
                    ( { model
                        | mode = Dialog FinishTutorial
                        , level = model.level + 1
                        , cntTask = GoToShop
                        , bag = addCoin model.bag 50
                        , unlockShop = True
                        , npclist = model.npclist |> updateBeaten
                      }
                    , Cmd.none
                    )

                _ ->
                    if followTutorial msg k && model.board.boardState == NoActions && model.board.turn == PlayerTurn then
                        { model | mode = Tutorial (k + 1) } |> updateBoardGame msg

                    else
                        ( model, Cmd.none )
