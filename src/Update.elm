module Update exposing (update)

import Board exposing (..)
import Data exposing (..)
import HeroAttack exposing (checkAttack, generateDamage, selectedHero, unselectedHero)
import Html.Attributes exposing (target)
import Message exposing (..)
import Model exposing (Model)
import String exposing (endsWith)
import ViewAllEnemy exposing (getEnemy)
import ShortestPath exposing (leastPath)


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

        Hit False ->
            case model.board.turn of
                HeroTurn ->
                    ( model, generateDamage )

                EnemyTurn ->
                    ( model, Cmd.none )

        EndTurn ->
            ( { model | board = turnEnemy model.board, heroes = List.map resetEnergy model.heroes }, Cmd.none )

        Tick elapsed ->
            case model.board.turn of
                HeroTurn ->
                    ( model, Cmd.none )

                EnemyTurn ->
                    ( moveEnemy { model | time = model.time + elapsed / 1000 } |> checkTurn, Cmd.none )

        GetCritical critical ->
            ( checkAttack model critical, Cmd.none )

        _ ->
            ( model, Cmd.none )


turnEnemy : Board -> Board
turnEnemy board =
    { board | turn = EnemyTurn, enemies = List.map (\enemy -> { enemy | done = False, steps = 2 }) board.enemies }


resetEnergy : Hero -> Hero
resetEnergy hero =
    case hero.class of
        Warrior ->
            { hero | energy = 5 }

        Archer ->
            { hero | energy = 5 }

        Mage ->
            { hero | energy = 5 }

        Assassin ->
            { hero | energy = 6 }


checkTurn : Model -> Model
checkTurn model =
    let
        board =
            model.board
    in
    if List.all (\enemy -> enemy.done) model.board.enemies then
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
            if legalHeroMove model.board model.heroes hero dr && hero.energy > 1 then
                let
                    currEnergy =
                        hero.energy
                in
                { model | heroes = { hero | pos = vecAdd hero.pos dr, energy = currEnergy - 2 } :: unselectedHero model.heroes }

            else
                model



-- To be rewritten later


moveEnemy : Model -> Model
moveEnemy model =
    let
        board =
            model.board

        ( newEnemy, newmodel ) =
            moveEnemyList model board.enemies
    in
    { newmodel | board = { board | enemies = newEnemy } }


moveEnemyList : Model -> List Enemy -> ( List Enemy, Model )
moveEnemyList model enemy_list =
    case enemy_list of
        [] ->
            ( [], model )

        enemy :: restEnemy ->
            let
                ( movedEnemy, newmodel ) =
                    --moveOneEnemy model enemy
                    moveSmartWarrior model enemy

                ( newrestEnemy, nnewmodel ) =
                    moveEnemyList newmodel restEnemy
            in
            ( movedEnemy :: newrestEnemy, nnewmodel )


moveSmartWarrior : Model -> Enemy  -> ( Enemy, Model )
moveSmartWarrior model enemy =
    let 
        route = leastPath enemy model.board model.heroes
    in
    case route of
        [] ->
            if  not enemy.done then
                (  { enemy | done = True}
                , { model | time = 0
                            , heroes = List.map (enermyWarriorAttack enemy.pos 5 0) model.heroes
                                       |> List.filter (\x -> ( x.health > 0 ) )
                            } )
            else
                ( enemy, model )

        a :: r_lst ->
            if model.time > 0.5 && not enemy.done then
                ( (checkEnemyDone { enemy | steps = enemy.steps - 1 , pos = a }), { model | time = 0 } )
            else
                ( enemy, model )


enermyWarriorAttack : Pos -> Int -> Int -> Hero -> Hero
enermyWarriorAttack my_enemy_pos damage critical hero =
    let
        newHealth =
            hero.health - damage - critical
    in
    if isWarriorAttackRange hero.pos my_enemy_pos then
        { hero | health = newHealth }
    else
        hero




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
    List.member (vecAdd hero.pos dr) board.map && not (List.member (vecAdd hero.pos dr) (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemies))


legalEnemyMove : Board -> List Hero -> Enemy -> Bool
legalEnemyMove board hero_list enemy =
    List.member enemy.pos board.map && not (List.member enemy.pos (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemies))


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



-- wait for more scene


checkEnd : Model -> Model
checkEnd model =
    if List.length model.heroes == 0 then
        model

    else if List.length model.board.enemies == 0 then
        model

    else
        model
