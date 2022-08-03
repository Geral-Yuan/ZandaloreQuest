module RpgCharacter exposing (moveCharacter)

{-| This file fills functions related to RPG character.


# Function

@docs moveCharacter

-}

import Type exposing (Dir(..), RpgCharacter)


{-| This function will move the character according to the arrow key pressed
-}
moveCharacter : RpgCharacter -> Float -> RpgCharacter
moveCharacter character dt =
    List.foldr (moveCharacterDir dt) character [ ( character.moveLeft, Left ), ( character.moveRight, Right ), ( character.moveUp, Up ), ( character.moveDown, Down ) ]


moveCharacterDir : Float -> ( Bool, Dir ) -> RpgCharacter -> RpgCharacter
moveCharacterDir dt ( on, dir ) character =
    let
        newFaceDir =
            case dir of
                Left ->
                    Left

                Right ->
                    Right

                _ ->
                    character.faceDir
    in
    if on then
        { character | pos = newCharacterPos character.pos dir (character.speed * dt), faceDir = newFaceDir }

    else
        character


newCharacterPos : ( Float, Float ) -> Dir -> Float -> ( Float, Float )
newCharacterPos ( px, py ) dir ds =
    case dir of
        Left ->
            ( px - ds, py )

        Right ->
            ( px + ds, py )

        Up ->
            ( px, py - ds / 1.2 )

        Down ->
            ( px, py + ds / 1.2 )
