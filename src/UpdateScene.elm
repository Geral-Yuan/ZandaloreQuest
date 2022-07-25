module UpdateScene exposing (checkLeaveCastle, checkLeaveDungeon, checkLeaveDungeon2, checkLeaveShop)

import Data exposing (FailToDo(..), GameMode(..), Scene(..))
import Message exposing (Msg(..))
import Model exposing (Model)


checkLeaveShop : Model -> Model
checkLeaveShop model =
    let
        character =
            model.character

        ( x, y ) =
            character.pos
    in
    if x > 740 && x < 930 && y > 830 then
        { model | mode = Castle, character = { character | width = 64, height = 64, pos = ( 1632, 802 ), speed = 500 } }

    else
        model


checkLeaveCastle : Model -> Model
checkLeaveCastle model =
    let
        character =
            model.character

        ( x, y ) =
            character.pos
    in
    if x > 1530 && x < 1690 && y < 810 && y > 780 then
        if model.unlockShop then
            { model | mode = Shop, character = { character | width = 100, height = 100, pos = ( 782, 882 ), speed = 800 } }

        else if Tuple.first model.popUpHint == Noop then
            model

        else
            { model | popUpHint = ( FailtoEnter ShopScene, 0 ) }

    else if x > 930 && x < 1080 && y <= 430 && y > 380 then
        if model.unlockDungeon then
            { model | mode = Dungeon, character = { character | pos = ( 1002, 962 ) } }

        else if Tuple.first model.popUpHint == Noop then
            model

        else
            { model | popUpHint = ( FailtoEnter DungeonScene, 0 ) }

    else if x > 320 && x < 440 && y < 810 && y > 780 then
        if model.unlockDungeon2 then
            { model | mode = Dungeon2, character = { character | pos = ( 1002, 962 ) } }

        else if Tuple.first model.popUpHint == Noop then
            model

        else
            { model | popUpHint = ( FailtoEnter Dungeon2Scene, 0 ) }

    else
        model


checkLeaveDungeon : Model -> Model
checkLeaveDungeon model =
    let
        character =
            model.character

        ( x, y ) =
            character.pos
    in
    if y > 850 && x > 900 && x < 1100 then
        { model | mode = Castle, character = { character | pos = ( 1007, 407 ) } }

    else
        model


checkLeaveDungeon2 : Model -> Model
checkLeaveDungeon2 model =
    let
        character =
            model.character

        ( x, y ) =
            character.pos
    in
    if y > 850 && x > 900 && x < 1100 then
        { model | mode = Castle, character = { character | pos = ( 377, 802 ) } }

    else
        model
