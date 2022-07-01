module Update exposing (update)

import Board exposing (..)
import Data exposing (..)
import HeroAttack exposing (checkAttack, generateDamage, selectedHero, unselectedHero)
import Html.Attributes exposing (list, target)
import Message exposing (..)
import Model exposing (Model)
import ShortestPath exposing (leastPath)
import String exposing (endsWith)
import ViewAllEnemy exposing (getEnemy)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize width height ->
            ( { model | size = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

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
            let
                nheroes =
                    List.map resetEnergy model.heroes

                nnheroes =
                    List.map deselectHeroes nheroes
            in
            ( { model | board = turnEnemy model.board, heroes = nnheroes }, Cmd.none )

        Tick elapsed ->
            let
                nboard =
                    highlightCells model
            in
            case model.board.turn of
                HeroTurn ->
                    ( { model | board = nboard }, Cmd.none )

                EnemyTurn ->
                    ( moveEnemy { model | time = model.time + elapsed / 1000, board = nboard } |> checkTurn, Cmd.none )

        GetCritical critical ->
            ( checkAttack model critical, Cmd.none )

        Click x y ->
            let
                ( w, h ) =
                    model.size
            in
            if w / h > pixelWidth / pixelHeight then
                ( { model | clickPos = ( (x - 1 / 2) * w / h * pixelHeight + 1 / 2 * pixelWidth, y * pixelHeight * w / h ) }, Cmd.none )

            else
                ( { model | clickPos = ( x * pixelWidth, (y - 1 / 2 * h / w) * pixelWidth + 1 / 2 * pixelHeight ) }, Cmd.none )

        GetViewport { viewport } ->
            ( { model
                | size =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none
            )

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


deselectHeroes : Hero -> Hero
deselectHeroes hero =
    { hero | selected = False }


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


moveSmartWarrior : Model -> Enemy -> ( Enemy, Model )
moveSmartWarrior model enemy =
    let
        route =
            leastPath enemy model.board model.heroes
    in
    case route of
        [] ->
            if not enemy.done then
                ( { enemy | done = True }
                , { model
                    | time = 0
                    , heroes =
                        List.map (enermyWarriorAttack enemy.pos 5 0) model.heroes
                            |> List.filter (\x -> x.health > 0)
                  }
                )

            else
                ( enemy, model )

        a :: r_lst ->
            if model.time > 0.5 && not enemy.done then
                ( checkEnemyDone { enemy | steps = enemy.steps - 1, pos = a }, { model | time = 0 } )

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


checkEnemyDone : Enemy -> Enemy
checkEnemyDone enemy =
    if enemy.steps == 0 then
        { enemy | done = True }

    else
        enemy


legalHeroMove : Board -> List Hero -> Hero -> Pos -> Bool
legalHeroMove board hero_list hero dr =
    List.member (vecAdd hero.pos dr) board.map && not (List.member (vecAdd hero.pos dr) (board.barrier ++ List.map .pos hero_list ++ List.map .pos board.enemies))


highlightCells : Model -> Board
highlightCells model =
    model.board
        |> heroMoveable model.heroes
        |> heroAttackable model.heroes


heroMoveable : List Hero -> Board -> Board
heroMoveable hero_list board =
    let
        sample_hero =
            Hero Warrior ( 100, 100 ) 1 1 1 1 False 100

        selected =
            selectedHero hero_list
                |> Maybe.withDefault sample_hero

        {- neighbour =
           vecAdd selected.pos ( 1, 0 )
        -}
        neighbour =
            List.map (vecAdd selected.pos) [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]

        can_move =
            neighbour
                |> List.filter (myListMember "in" board.map)
                |> List.filter (myListMember "not in" board.barrier)
                |> List.filter (myListMember "not in" (List.map .pos hero_list))
                |> List.filter (myListMember "not in" (List.map .pos board.enemies))
    in
    { board | moveable = can_move }


heroAttackable : List Hero -> Board -> Board
heroAttackable hero_list board =
    let
        sample_hero =
            Hero Warrior ( 100, 100 ) 1 1 1 1 False 100

        selected =
            selectedHero hero_list
                |> Maybe.withDefault sample_hero

        neighbour =
            [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]
    in
    case selected.class of
        Warrior ->
            let
                attack_range =
                    List.map (vecAdd selected.pos) neighbour

                can_attack =
                    attack_range
                        |> List.filter (myListMember "in" board.map)
                        |> List.filter (myListMember "not in" board.barrier)
                        |> List.filter (myListMember "not in" (List.map .pos hero_list))
                        |> List.filter (myListMember "in" (List.map .pos board.enemies))
            in
            { board | attackable = can_attack }

        Assassin ->
            let
                attack_range =
                    List.map (vecAdd selected.pos) neighbour

                can_attack =
                    attack_range
                        |> List.filter (myListMember "in" board.map)
                        |> List.filter (myListMember "not in" board.barrier)
                        |> List.filter (myListMember "not in" (List.map .pos hero_list))
                        |> List.filter (myListMember "in" (List.map .pos board.enemies))
            in
            { board | attackable = can_attack }

        Archer ->
            board

        {- let
               attack_range =
                   List.map (vecAdd selected.pos) (cartesianProduct vecScale (List.range 1 10) neighbour)

               can_attack =
                   attack_range
                       |> List.filter (myListMember "in" board.map)
                       |> List.filter (myListMember "not in" board.barrier)
                       |> List.filter (myListMember "not in" (List.map .pos hero_list))
                       |> List.filter (myListMember "in" (List.map .pos board.enemies))
           in
           { board | attackable = can_attack }
        -}
        Mage ->
            board


myListMember : String -> List a -> a -> Bool
myListMember s lis x =
    if s == "in" then
        List.member x lis

    else
        not (List.member x lis)


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
