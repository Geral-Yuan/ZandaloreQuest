module RpgCharacter exposing (..)

import Data exposing (Dir(..))


type alias RpgCharacter =
    { pos : ( Float, Float )
    , moveLeft : Bool
    , moveRight : Bool
    , moveUp : Bool
    , moveDown : Bool
    , faceDir : Dir
    , latestDir : Dir
    , height : Float
    , width : Float
    , speed : Float
    , move_range : ( Float, Float ) -- right bound and bottom bound
    }


moveCharacter : RpgCharacter -> Float -> RpgCharacter
moveCharacter character dt =
    case [ character.moveLeft, character.moveRight, character.moveUp, character.moveDown ] of
        [ True, False, False, False ] ->
            if isLegalMoveCharacter character Left then
                { character | pos = newCharacterPos character.pos Left (character.speed * dt), latestDir = Left, faceDir = Left }

            else
                character

        [ False, True, False, False ] ->
            if isLegalMoveCharacter character Right then
                { character | pos = newCharacterPos character.pos Right (character.speed * dt), latestDir = Right, faceDir = Right }

            else
                character

        [ False, False, True, False ] ->
            if isLegalMoveCharacter character Up then
                { character | pos = newCharacterPos character.pos Up (character.speed * dt), latestDir = Up }

            else
                character

        [ False, False, False, True ] ->
            if isLegalMoveCharacter character Down then
                { character | pos = newCharacterPos character.pos Down (character.speed * dt), latestDir = Down }

            else
                character

        [ True, True, False, False ] ->
            -- make it smoother between Left Right
            if isLegalMoveCharacter character character.latestDir then
                { character | pos = newCharacterPos character.pos character.latestDir (character.speed * dt) }

            else
                character

        [ True, False, True, False ] ->
            -- make it smoother between Left Up
            if isLegalMoveCharacter character character.latestDir then
                { character | pos = newCharacterPos character.pos character.latestDir (character.speed * dt) }

            else
                character

        [ True, False, False, True ] ->
            -- make it smoother between Left Down
            if isLegalMoveCharacter character character.latestDir then
                { character | pos = newCharacterPos character.pos character.latestDir (character.speed * dt) }

            else
                character

        [ False, True, False, True ] ->
            -- make it smoother between Right Down
            if isLegalMoveCharacter character character.latestDir then
                { character | pos = newCharacterPos character.pos character.latestDir (character.speed * dt) }

            else
                character

        [ False, True, True, False ] ->
            -- make it smoother between Right Up
            if isLegalMoveCharacter character character.latestDir then
                { character | pos = newCharacterPos character.pos character.latestDir (character.speed * dt) }

            else
                character

        [ False, False, True, True ] ->
            -- make it smoother between Down Up
            if isLegalMoveCharacter character character.latestDir then
                { character | pos = newCharacterPos character.pos character.latestDir (character.speed * dt) }

            else
                character

        _ ->
            character


isLegalMoveCharacter : RpgCharacter -> Dir -> Bool
isLegalMoveCharacter { pos, width, height, move_range } dir =
    case dir of
        Left ->
            if Tuple.first pos <= 0 then
                False

            else
                True

        Right ->
            if Tuple.first pos + width >= Tuple.first move_range then
                --the right bound of the game screen
                False

            else
                True

        Up ->
            if Tuple.second pos <= 0 then
                --the top bound of the game screen
                False

            else
                True

        Down ->
            if Tuple.second pos + height >= Tuple.second move_range then
                --the bottom bound of the game screen
                False

            else
                True

        _ ->
            False


newCharacterPos : ( Float, Float ) -> Dir -> Float -> ( Float, Float )
newCharacterPos ( px, py ) dir ds =
    case dir of
        Left ->
            ( px - ds, py )

        Right ->
            ( px + ds, py )

        Up ->
            ( px, py - ds )

        Down ->
            ( px, py + ds )

        _ ->
            ( px, py )