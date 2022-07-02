module Update exposing (update)

import Data exposing (..)
import Message exposing (Msg(..))
import Model exposing (Model)
import UpdateBoard exposing (updateBoard)
import HeroAttack exposing (generateDamage)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.mode of
        BoardGame ->
            { model | board = updateBoard msg model.board }
                |> resize msg
                |> checkClick msg
                |> getviewport msg
                |> checkHit msg

        _ ->
            ( model, Cmd.none )


resize : Msg -> Model -> Model
resize msg model =
    case msg of
        Resize width height ->
            { model | size = ( toFloat width, toFloat height ) }

        _ ->
            model


checkHit : Msg -> Model -> ( Model, Cmd Msg )
checkHit msg model =
    case msg of
        Hit False ->
            case model.board.turn of
                HeroTurn ->
                    ( model, generateDamage )

                EnemyTurn ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


checkClick : Msg -> Model -> Model
checkClick msg model =
    case msg of
        Click x y ->
            let
                ( w, h ) =
                    model.size
            in
            if w / h > pixelWidth / pixelHeight then
                { model | clickPos = ( (x - 1 / 2) * w / h * pixelHeight + 1 / 2 * pixelWidth, y * pixelHeight * w / h ) }

            else
                { model | clickPos = ( x * pixelWidth, (y - 1 / 2 * h / w) * pixelWidth + 1 / 2 * pixelHeight ) }

        _ ->
            model


getviewport : Msg -> Model -> Model
getviewport msg model =
    case msg of
        GetViewport { viewport } ->
            { model
                | size =
                    ( viewport.width
                    , viewport.height
                    )
            }

        _ ->
            model
