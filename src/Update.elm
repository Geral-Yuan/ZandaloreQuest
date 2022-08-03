module Update exposing (update)

{-| This file fills functions related to the main update function.


# Function

@docs update

-}

import Board exposing (initBoard)
import Data exposing (class2Index, findChosenHero, index2Class, initialHeroes)
import Message exposing (Msg(..))
import Type exposing (Class, FailToDo(..), GameMode(..), Hero, Model, Task(..))
import UpdateBoard exposing (updateBoardGame)
import UpdateCharacter exposing (updateCharacter)
import UpdateRPG exposing (updateRPG)
import UpdateShop exposing (updateShop)
import UpdateTutorial exposing (updateTutorial)
import ViewConst exposing (pixelHeight, pixelWidth)


{-| This is the main update function.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( nmodel, ncmd ) =
            case model.mode of
                Tutorial k ->
                    updateTutorial msg k model

                BoardGame ->
                    updateBoardGame msg model

                Summary ->
                    updateSummary msg model

                Logo ->
                    ( updateScene msg model, Cmd.none )

                Dialog task ->
                    updateDialog msg task model

                HeroChoose ->
                    ( model
                        |> checkChooseClick msg
                        |> checkConfirm msg
                    , Cmd.none
                    )

                Encyclopedia class ->
                    updateEncyclopedia msg model class

                BuyingItems ->
                    updateShop msg model

                UpgradePage ->
                    updateShop msg model

                DrawHero _ ->
                    updateShop msg model

                _ ->
                    updateRPG msg model
                        |> updateCharacter msg
    in
    ( nmodel
        |> updatePopUpHint msg
        |> resize msg
        |> getviewport msg
    , ncmd
    )


updatePopUpHint : Msg -> Model -> Model
updatePopUpHint msg model =
    let
        ( hint, time ) =
            model.popUpHint

        board =
            model.board

        nmodel =
            if Tuple.first board.popUpHint /= Noop then
                if hint == Noop then
                    { model | popUpHint = board.popUpHint, board = { board | popUpHint = ( Noop, 0 ) } }

                else
                    { model | board = { board | popUpHint = ( Noop, 0 ) } }

            else
                model
    in
    case msg of
        Tick elapsed ->
            if time > 2 then
                { nmodel | popUpHint = ( Noop, 0 ) }

            else if hint /= Noop then
                { nmodel | popUpHint = ( hint, time + elapsed / 1000 ) }

            else
                nmodel

        _ ->
            nmodel


updateEncyclopedia : Msg -> Model -> Class -> ( Model, Cmd Msg )
updateEncyclopedia msg model class =
    case msg of
        Back ->
            ( { model | mode = model.previousMode }, Cmd.none )

        RightEncyclopedia ->
            ( { model | mode = Encyclopedia (index2Class (modBy 6 (class2Index class) + 1)) }, Cmd.none )

        LeftEncyclopedia ->
            ( { model | mode = Encyclopedia (index2Class (modBy 6 (class2Index class - 2) + 1)) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateSummary : Msg -> Model -> ( Model, Cmd Msg )
updateSummary msg model =
    case msg of
        Click _ _ ->
            ( { model | mode = model.previousMode, level = model.level + 1 }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateDialog : Msg -> Task -> Model -> ( Model, Cmd Msg )
updateDialog msg task model =
    case task of
        MeetElder ->
            case msg of
                Click _ _ ->
                    ( { model | mode = Tutorial 1, board = initBoard initialHeroes 0, chosenHero = [] }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FinishTutorial ->
            case msg of
                Click _ _ ->
                    ( { model | mode = Castle }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        --To be changed when it's other tasks
        _ ->
            case msg of
                Click _ _ ->
                    ( { model | mode = HeroChoose }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


checkChooseClick : Msg -> Model -> Model
checkChooseClick msg model =
    case msg of
        Click x y ->
            let
                ( w, h ) =
                    model.size

                clickpos =
                    if w / h > pixelWidth / pixelHeight then
                        ( (x - 1 / 2) * w / h * pixelHeight + 1 / 2 * pixelWidth, y * pixelHeight * w / h )

                    else
                        ( x * pixelWidth, (y - 1 / 2 * h / w) * pixelWidth + 1 / 2 * pixelHeight )

                chosenidx =
                    findChosenHero clickpos

                index =
                    if List.map Tuple.second model.indexedheroes |> List.member chosenidx then
                        chosenidx

                    else
                        0
            in
            if index > 0 && index <= 6 then
                if List.member index model.chosenHero then
                    { model | chosenHero = List.filter (\heroindex -> heroindex /= index) model.chosenHero }

                else if List.length model.chosenHero < 3 then
                    { model | chosenHero = index :: model.chosenHero }

                else
                    model

            else
                model

        _ ->
            model


checkConfirm : Msg -> Model -> Model
checkConfirm msg model =
    let
        level =
            model.level
    in
    case msg of
        Confirm ->
            if List.length model.chosenHero == 3 then
                { model | mode = BoardGame, board = initBoard (confirmHeroes model) level, chosenHero = [] }

            else
                model

        _ ->
            model


confirmHeroes : Model -> List Hero
confirmHeroes model =
    let
        chosenIndexedHeroes =
            List.filter (\( _, y ) -> List.member y model.chosenHero) model.indexedheroes

        sortedHeroes =
            Tuple.first (List.unzip (List.sortBy (\( _, y ) -> y) chosenIndexedHeroes))
    in
    initIndexOnBoard sortedHeroes


initIndexOnBoard : List Hero -> List Hero
initIndexOnBoard heroes =
    case List.reverse heroes of
        [] ->
            []

        lastHero :: rest ->
            initIndexOnBoard (List.reverse rest) ++ [ { lastHero | indexOnBoard = List.length heroes } ]


updateScene : Msg -> Model -> Model
updateScene msg model =
    case msg of
        Tick elapsed ->
            { model | time = model.time + elapsed / 1000 }
                |> checkLogoEnd

        Enter False ->
            { model | mode = Castle }

        _ ->
            model


checkLogoEnd : Model -> Model
checkLogoEnd model =
    if (model.time > 6.1) && (model.mode == Logo) then
        { model | mode = Castle }

    else
        model


resize : Msg -> Model -> Model
resize msg model =
    case msg of
        Resize width height ->
            { model | size = ( toFloat width, toFloat height ) }

        _ ->
            model


getviewport : Msg -> Model -> Model
getviewport msg model =
    case msg of
        GetViewport { viewport } ->
            { model
                | size =
                    ( viewport.width
                    , viewport.height
                    )
            }

        _ ->
            model
