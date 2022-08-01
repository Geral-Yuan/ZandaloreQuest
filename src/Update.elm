module Update exposing (update)

import Board exposing (initBoard)
import Data exposing (allSampleHeroes, class2Index, findChosenHero, index2Class, initialHeroes, mode2Scene, pixelHeight, pixelWidth)
import Message exposing (Msg(..))
import NPC exposing (allNPC)
import RpgCharacter exposing (moveCharacter)
import Svg.Attributes exposing (mode)
import Type exposing (..)
import UpdateBoard exposing (updateBoardGame)
import UpdateScene exposing (checkLeaveCastle, checkLeaveDungeon, checkLeaveDungeon2, checkLeaveShop)
import UpdateShop exposing (updateShop)
import UpdateTutorial exposing (updateTutorial)
import ViewNPCTask exposing (checkTalkRange)


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


updateCharacter : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateCharacter msg ( model, cmd ) =
    case msg of
        Tick elapse ->
            let
                newCharacter =
                    moveCharacter model.character (elapse / 1000)
            in
            if isReachable model.mode newCharacter.pos model.npclist then
                ( { model | character = newCharacter }, cmd )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


isReachable : GameMode -> ( Float, Float ) -> List NPC -> Bool
isReachable mode ( x, y ) npclist =
    not (List.foldr (||) False (List.map (npcCollisionRange ( x, y )) (npclist |> List.filter (\npc -> npc.scene == mode2Scene mode))))
        && List.foldr (||) False (List.map (reachableRectangle ( x, y )) (sceneReachable mode))


reachableRectangle : ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ) ) -> Bool
reachableRectangle ( x, y ) ( ( xmin, xmax ), ( ymin, ymax ) ) =
    x > xmin && x <= xmax && y > ymin && y <= ymax


sceneReachable : GameMode -> List ( ( Float, Float ), ( Float, Float ) )
sceneReachable mode =
    case mode of
        Castle ->
            [ ( ( 310, 1692 ), ( 782, 812 ) )
            , ( ( 572, 1427 ), ( 407, 782 ) )
            , ( ( 732, 1272 ), ( 382, 407 ) )
            , ( ( 572, 667 ), ( 197, 407 ) )
            , ( ( 1332, 1427 ), ( 197, 407 ) )
            , ( ( 322, 1692 ), ( 167, 197 ) )
            , ( ( 322, 497 ), ( 197, 602 ) )
            , ( ( 1492, 1692 ), ( 197, 602 ) )
            , ( ( 322, 447 ), ( 32, 167 ) )
            , ( ( 1557, 1692 ), ( 32, 167 ) )
            ]

        Shop ->
            [ ( ( 682, 902 ), ( 782, 902 ) )
            , ( ( 392, 1217 ), ( 582, 782 ) )
            , ( ( 392, 462 ), ( 410, 582 ) )
            ]

        Dungeon ->
            [ ( ( 502, 1542 ), ( 241, 914 ) ) ]

        Dungeon2 ->
            [ ( ( 502, 1542 ), ( 241, 914 ) ) ]

        _ ->
            []


npcCollisionRange : ( Float, Float ) -> NPC -> Bool
npcCollisionRange ( x, y ) npc =
    let
        ( nx, ny ) =
            npc.position

        ( nw, nh ) =
            npc.size
    in
    x > nx - nw + 20 && x < nx + nw - 20 && y > ny - nh && y < ny + nh - 30


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


updateRPG : Msg -> Model -> ( Model, Cmd Msg )
updateRPG msg model =
    let
        character =
            model.character

        ( x, y ) =
            character.pos

        currCoins =
            model.bag.coins

        bag =
            model.bag

        currHeroes =
            model.indexedheroes
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

        UpgradeHealth ->
            if model.bag.coins > 49 then
                ( { model | bag = { bag | coins = currCoins - 50 }, indexedheroes = List.map updateHealth currHeroes }, Cmd.none )

            else
                ( model, Cmd.none )

        UpgradeDamage ->
            if model.bag.coins > 49 then
                ( { model | bag = { bag | coins = currCoins - 50 }, indexedheroes = List.map updateDamage currHeroes }, Cmd.none )

            else
                ( model, Cmd.none )

        ExitShop ->
            ( { model | mode = Shop }, Cmd.none )

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


