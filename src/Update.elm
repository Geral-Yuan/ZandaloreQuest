module Update exposing (update)

import Board exposing (..)
import Data exposing (..)
import Html.Attributes exposing (target)
import Message exposing (..)
import Model exposing (Model)
import String exposing (endsWith)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Key dir False ->
            case model.board.turn of
                HeroTurn ->
                    ( moveHero model dir, Cmd.none )

                EnemyTurn ->
                    ( model, Cmd.none )

        Select class False ->
            case model.board.turn of
                HeroTurn ->
                    ( selectHero model class, Cmd.none )

                EnemyTurn ->
                    ( model, Cmd.none )

        EndTurn ->
            ( { model | board = turnEnemy model.board }, Cmd.none )

        Tick elapsed ->
            case model.board.turn of
                HeroTurn ->
                    ( model, Cmd.none )

                EnemyTurn ->
                    ( moveEnemy { model | time = model.time + elapsed / 1000 } |> checkTurn, Cmd.none )

        _ ->
            ( model, Cmd.none )


turnEnemy : Board -> Board
turnEnemy board =
    { board | turn = EnemyTurn, enemy = List.map (\enemy -> { enemy | done = False, steps = 2 }) board.enemy }


checkTurn : Model -> Model
checkTurn model =
    let
        board =
            model.board
    in
    if List.all (\enemy -> enemy.done) model.board.enemy then
        { model | board = { board | turn = HeroTurn } }

    else
        model


moveHero : Model -> Dir -> Model
moveHero model dir =
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
    case selectedHero model.heroes of
        Nothing ->
            model

        Just hero ->
            if legalHeroMove model.board model.heroes hero dr then
                { model | heroes = { hero | pos = vecAdd hero.pos dr } :: unselectedHero model.heroes }

            else
                model



-- To be rewritten later


moveEnemy : Model -> Model
moveEnemy model =
    let
        board =
            model.board

        ( newEnemy, newmodel ) =
            moveEnemyList model board.enemy
    in
    { newmodel | board = { board | enemy = newEnemy } }


moveEnemyList : Model -> List Enemy -> ( List Enemy, Model )
moveEnemyList model enemy_list =
    case enemy_list of
        [] ->
            ( [], model )

        enemy :: restEnemy ->
            let
                ( movedEnemy, newmodel ) =
                    moveOneEnemy model enemy

                ( newrestEnemy, nnewmodel ) =
                    moveEnemyList newmodel restEnemy
            in
            ( movedEnemy :: newrestEnemy, nnewmodel )


moveOneEnemy : Model -> Enemy -> ( Enemy, Model )
moveOneEnemy model enemy =
    case targetHero model.heroes enemy of
        Nothing ->
            ( enemy, model )

        Just hero ->
            if model.time > 0.5 && not enemy.done then
                ( moveEnemyOrient model (checkEnemyDone { enemy | steps = enemy.steps - 1 }) (detOrientation hero.pos enemy.pos), { model | time = 0 } )

            else
                ( enemy, model )


moveEnemyOrient : Model -> Enemy -> Orientation -> Enemy
moveEnemyOrient model enemy orient =
    let
        newEnemy =
            case orient of
                RightDown ->
                    { enemy | pos = vecAdd enemy.pos ( 1, 0 ) }

                LeftDown ->
                    { enemy | pos = vecAdd enemy.pos ( 0, 1 ) }

                Left ->
                    { enemy | pos = vecAdd enemy.pos ( -1, 1 ) }

                LeftUp ->
                    { enemy | pos = vecAdd enemy.pos ( -1, 0 ) }

                RightUp ->
                    { enemy | pos = vecAdd enemy.pos ( 0, -1 ) }

                Right ->
                    { enemy | pos = vecAdd enemy.pos ( 1, -1 ) }
    in
    if legalEnemyMove model.board model.heroes newEnemy then
        newEnemy

    else
        enemy


checkEnemyDone : Enemy -> Enemy
checkEnemyDone enemy =
    if enemy.steps == 0 then
        { enemy | done = True }

    else
        enemy


targetHero : List Hero -> Enemy -> Maybe Hero
targetHero hero_list enemy =
    case leastdistance (List.map .pos hero_list) enemy.pos of
        Just dis ->
            List.head (List.filter (\hero -> distance enemy.pos hero.pos == dis) hero_list)

        _ ->
            Nothing


legalHeroMove : Board -> List Hero -> Hero -> Pos -> Bool
legalHeroMove board hero_list hero dr =
    List.member (vecAdd hero.pos dr) board.map && not (List.member (vecAdd hero.pos dr) (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemy))


legalEnemyMove : Board -> List Hero -> Enemy -> Bool
legalEnemyMove board hero_list enemy =
    List.member enemy.pos board.map && not (List.member enemy.pos (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemy))


selectHero : Model -> Class -> Model
selectHero model class =
    let
        ( wantedHero, unwantedHero ) =
            List.partition (\hero -> hero.class == class) model.heroes

        newwantedHero =
            List.map (\hero -> { hero | selected = True }) wantedHero

        newunwantedHero =
            List.map (\hero -> { hero | selected = False }) unwantedHero
    in
    { model | heroes = newwantedHero ++ newunwantedHero }


selectedHero : List Hero -> Maybe Hero
selectedHero hero_list =
    List.head (List.filter (\hero -> hero.selected) hero_list)


unselectedHero : List Hero -> List Hero
unselectedHero hero_list =
    List.filter (\hero -> not hero.selected) hero_list
