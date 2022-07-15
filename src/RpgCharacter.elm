module RpgCharacter exposing (..)

import Data exposing (Dir(..))
import Html.Events exposing (on)


type CharacterState
    = Still
    | MovingLeft
    | MovingRight


type alias RpgCharacter =
    { pos : ( Float, Float )
    , moveLeft : Bool
    , moveRight : Bool
    , moveUp : Bool
    , moveDown : Bool
    , state : CharacterState
    , faceDir : Dir

    --    , latestDir : Dir
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
    if on && character.faceDir == Left then
        { character | pos = newCharacterPos character.pos dir (character.speed * dt), state = MovingLeft }

    else if on && character.faceDir == Right then
        { character | pos = newCharacterPos character.pos dir (character.speed * dt), state = MovingRight }

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
