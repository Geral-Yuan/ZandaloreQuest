module AttackCommon exposing (isWarriorAttackRange)

import Data exposing (Pos)
isWarriorAttackRange : Pos -> Pos -> Bool
isWarriorAttackRange attacked me =
    let
        ( x, y ) =
            attacked
    in
    if
        List.member me
            [ ( x + 1, y )
            , ( x, y + 1 )
            , ( x + 1, y - 1 )
            , ( x, y - 1 )
            , ( x - 1, y )
            , ( x - 1, y + 1 )
            ]
    then
        True

    else
        False