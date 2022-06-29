module Update exposing (update)

import Board exposing (..)
import Data exposing (..)
import Html.Attributes exposing (target)
import Message exposing (..)
import Model exposing (Model)
import String exposing (endsWith)
import ViewAllEnemy exposing (getEnemy)


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
                    ( checkAttack model, Cmd.none )

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

        _ ->
            ( model, Cmd.none )


checkAttack : Model -> Model
checkAttack model =
    -- reduce the energy of a hero when player clicks h (hit) and check surroundings for enemies
    case selectedHero model.heroes of
        Nothing ->
            model

        Just hero ->
            if hero.energy > 2 then
                let
                    currEnergy =
                        hero.energy
                in
                { model | heroes = { hero | energy = currEnergy - 3 } :: unselectedHero model.heroes, board = checkForEnemy model }

            else
                model


checkForEnemy : Model -> Board
checkForEnemy model =
    -- check if there are enemies in the surroundings
    case selectedHero model.heroes of
        Nothing ->
            model.board

        Just hero ->
            let
                enemy =
                    { class = Data.Warrior
                    , pos = ( 6, 6 )
                    , health = 100
                    , damage = 15
                    , armour = 5
                    , steps = 0
                    , done = False
                    , indexOnBoard = 1
                    }

                board =
                    model.board

                enemy1 =
                    getEnemy enemy model.board.enemies 1

                enemy2 =
                    getEnemy enemy model.board.enemies 2

                enemy3 =
                    getEnemy enemy model.board.enemies 3
            in
            case hero.class of
                Warrior ->
                    { board | enemies = checkMelee hero.pos hero.damage enemy1 :: checkMelee hero.pos hero.damage enemy2 :: [ checkMelee hero.pos hero.damage enemy3 ] }

                Assassin ->
                    { board | enemies = checkMelee hero.pos hero.damage enemy1 :: checkMelee hero.pos hero.damage enemy2 :: [ checkMelee hero.pos hero.damage enemy3 ] }

                _ ->
                    model.board


checkMelee : Pos -> Int -> Enemy -> Enemy
checkMelee ( x, y ) damage enemy =
    -- for warriors and assassins classes
    -- if there are enemies within the 6 hexagons around their current location
    -- the enemies will receive the damage
    let
        newHealth =
            enemy.health - damage
    in
    if enemy.pos == ( x + 1, y ) then
        { enemy | health = newHealth }

    else if enemy.pos == ( x, y + 1 ) then
        { enemy | health = newHealth }

    else if enemy.pos == ( x + 1, y - 1 ) then
        { enemy | health = newHealth }

    else if enemy.pos == ( x, y - 1 ) then
        { enemy | health = newHealth }

    else if enemy.pos == ( x - 1, y ) then
        { enemy | health = newHealth }

    else if enemy.pos == ( x - 1, y + 1 ) then
        { enemy | health = newHealth }

    else
        enemy


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


selectedHero : List Hero -> Maybe Hero
selectedHero hero_list =
    List.head (List.filter (\hero -> hero.selected) hero_list)


unselectedHero : List Hero -> List Hero
unselectedHero hero_list =
    List.filter (\hero -> not hero.selected) hero_list
