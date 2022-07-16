module RpgCharacter exposing (..)

import Data exposing (Dir(..))
import Html.Events exposing (on)



-- type CharacterState
--     = Still
--     | MovingLeft
--     | MovingRight


type alias RpgCharacter =
    { pos : ( Float, Float )
    , moveLeft : Bool
    , moveRight : Bool
    , moveUp : Bool
    , moveDown : Bool

    --    , state : CharacterState
    , faceDir : Dir
    , height : Float
    , width : Float
    , speed : Float
    , move_range : ( Float, Float ) -- right bound and bottom bound
    }


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
            ( px, py - ds )

        Down ->
            ( px, py + ds )

        _ ->
            ( px, py )
