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

        _ ->
            ( model, Cmd.none )


turnEnemy : Board -> Board
turnEnemy board =
    { board | turn = EnemyTurn, enemy = List.map (\enemy -> { enemy | done = False, steps = 2 }) board.enemy }


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


legalHeroMove : Board -> List Hero -> Hero -> Pos -> Bool
legalHeroMove board hero_list hero dr =
    List.member (vecAdd hero.pos dr) board.map && not (List.member (vecAdd hero.pos dr) (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemy))


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
