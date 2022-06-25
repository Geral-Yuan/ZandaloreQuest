module Update exposing (update)

import Data exposing (..)
import Message exposing (..)
import Model exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Key dir False->
            ( moveChara model dir, Cmd.none )

        _ ->
            ( model, Cmd.none )


moveChara : Model -> Dir -> Model
moveChara model dir =
    let
        dr =
            case dir of
                W ->
                    ( -1, 0 )

                E ->
                    ( 0, -1 )

                D ->
                    ( 1, -1 )

                X ->
                    ( 1, 0 )

                Z ->
                    ( 0, 1 )

                A ->
                    ( -1, 1 )
    in
    case selectedChara model.characters of
        Nothing ->
            model

        Just chara ->
            { model | characters = { chara | pos = vecAdd chara.pos dr } :: unselectedChara model.characters }


selectedChara : List Character -> Maybe Character
selectedChara character_list =
    List.head (List.filter (\chara -> chara.selected) character_list)


unselectedChara : List Character -> List Character
unselectedChara character_list =
    List.filter (\chara -> not chara.selected) character_list
