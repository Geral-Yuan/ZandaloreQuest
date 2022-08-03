module UpdateCharacter exposing (updateCharacter)

{-| This file fills functions related to updating main character.


# Function

@docs updateCharacter

-}

import Data exposing (mode2Scene)
import Message exposing (Msg(..))
import RpgCharacter exposing (moveCharacter)
import Type exposing (GameMode(..), Model, NPC)


{-| This function will update the main character when walking, entering, talking
-}
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
            , ( ( 310, 1692 ), ( 167, 197 ) )
            , ( ( 310, 497 ), ( 197, 602 ) )
            , ( ( 1492, 1692 ), ( 197, 602 ) )
            , ( ( 310, 447 ), ( 32, 167 ) )
            , ( ( 1557, 1692 ), ( 32, 167 ) )
            ]

        Shop ->
            [ ( ( 682, 902 ), ( 782, 902 ) )
            , ( ( 392, 1217 ), ( 582, 782 ) )
            , ( ( 392, 462 ), ( 410, 582 ) )
            ]

        Dungeon ->
            [ ( ( 502, 1542 ), ( 241, 970 ) ) ]

        Dungeon2 ->
            [ ( ( 502, 1542 ), ( 241, 970 ) ) ]

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
