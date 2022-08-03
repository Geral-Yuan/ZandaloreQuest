module UpdateTurret exposing (actionTurret, checkCurrentTurret, checkTurretDone, updateTurretAttackable)

{-| This file fills functions related to turret action.


# Function

@docs actionTurret, checkCurrentTurret, checkTurretDone, updateTurretAttackable

-}

import Action exposing (attackedByHeroArcherRange)
import HeroAttack exposing (heroTurretAttack)
import ListOperation exposing (listDifference)
import Type exposing (Board, BoardState(..), Class(..), Hero, HeroState(..), Turn(..))


{-| This function will let the turret take action.
-}
actionTurret : Board -> Board
actionTurret board =
    let
        undoneTurret =
            List.filter (\turret -> (turret.class == Turret) && (turret.energy == 0)) board.heroes

        selectboard =
            case undoneTurret of
                [] ->
                    board

                turret :: _ ->
                    let
                        attackedenemy =
                            heroTurretAttack turret board
                    in
                    if attackedenemy /= board.enemies then
                        { board
                            | enemies = attackedenemy |> List.filter (\x -> x.health > 0)
                            , heroes = { turret | energy = -3, state = Attacking } :: listDifference board.heroes [ turret ]
                            , boardState = HeroAttack
                            , attackable = attackedByHeroArcherRange board turret.pos
                        }

                    else
                        { board
                            | heroes = { turret | energy = -3, state = Waiting } :: listDifference board.heroes [ turret ]
                            , attackable = attackedByHeroArcherRange board turret.pos
                        }
    in
    selectboard


{-| This function will detect which turret will be taking action at that moment.
-}
checkCurrentTurret : Board -> Board
checkCurrentTurret board =
    let
        undoneTurret =
            List.filter (\turret -> (turret.class == Turret) && (turret.energy /= -6)) board.heroes
    in
    case undoneTurret of
        [] ->
            { board | cntTurret = 0 }

        enemy :: _ ->
            { board | cntTurret = enemy.indexOnBoard }


{-| This function will update the turret attackable range.
-}
updateTurretAttackable : Board -> Board
updateTurretAttackable board =
    let
        maybeTurret =
            List.head (List.filter (\x -> x.indexOnBoard == board.cntTurret) board.heroes)
    in
    if board.turn == TurretTurn then
        case maybeTurret of
            Nothing ->
                { board | attackable = [] }

            Just turret ->
                let
                    realattackRange =
                        attackedByHeroArcherRange board turret.pos
                in
                { board | attackable = realattackRange }

    else
        board


{-| This function will prevent turret from attacking more.
-}
checkTurretDone : Hero -> Hero
checkTurretDone turret =
    if turret.energy == -3 && turret.state == Waiting && turret.class == Turret then
        { turret | energy = -6 }

    else
        turret
