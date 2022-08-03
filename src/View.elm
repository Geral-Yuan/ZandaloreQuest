module View exposing (view)

{-| This file fills functions related to the main view function.


# Function

@docs view

-}

import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Message exposing (Msg(..))
import Type exposing (GameMode(..), Model)
import ViewBoard exposing (viewBoard, viewTutorialBoard)
import ViewChoose exposing (viewHeroChoose)
import ViewDialog exposing (viewDialog)
import ViewEncyclopedia exposing (viewEncyclopedia)
import ViewScenes exposing (viewCastle, viewDungeon, viewDungeon2, viewScene0, viewSummary)
import ViewShop exposing (viewDrawnHero, viewShop, viewShopChoose, viewUpgradePage)


{-| This is the main view function.
-}
view : Model -> Html Msg
view model =
    let
        viewAll =
            case model.mode of
                Logo ->
                    viewScene0 model

                BoardGame ->
                    viewBoard model

                Summary ->
                    viewSummary model

                Castle ->
                    viewCastle model

                Shop ->
                    viewShop model

                HeroChoose ->
                    viewHeroChoose model

                BuyingItems ->
                    viewShopChoose model

                DrawHero class ->
                    viewDrawnHero model class

                UpgradePage ->
                    viewUpgradePage model

                --to be revised
                Dungeon ->
                    viewDungeon model

                Dungeon2 ->
                    viewDungeon2 model

                Tutorial k ->
                    viewTutorialBoard k model

                Dialog task ->
                    viewDialog task model

                Encyclopedia hero ->
                    viewEncyclopedia hero model
    in
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "background" "black"
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ viewAll
        ]
