module ViewShop exposing (..)

import Data exposing (Class(..), Hero, HeroState(..), Scene(..), pixelHeight, pixelWidth)
import Debug exposing (toString)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (Model)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import ViewNPCTask exposing (..)
import ViewScenes exposing (..)


viewShop : Model -> Html Msg
viewShop model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "black"
        ]
        (viewKeyGif
            ++ [ viewTask model
               , Svg.svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    ]
                    [ viewShopSvg
                    , viewTaskBoard
                    ]
               , viewCharacterPos model.character
               , viewTipForDir
               , viewTipForC
               , viewTipForEnter
               ]
            ++ List.concat (List.map viewSingleNPC (model.npclist |> List.filter (\x -> x.scene == ShopScene)))
            ++ [ viewRpgCharacter model.character ]
        )


viewShopSvg : Svg msg
viewShopSvg =
    Svg.image
        [ SvgAttr.width "1600"
        , SvgAttr.height "1000"
        , SvgAttr.x (toString (pixelWidth / 2 - 800))
        , SvgAttr.y (toString (pixelHeight / 2 - 500))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Shop.jpg"
        ]
        []


viewBuySvg : Svg msg
viewBuySvg =
    Svg.image
        [ SvgAttr.width "1600"
        , SvgAttr.height "1000"
        , SvgAttr.x (toString (pixelWidth / 2 - 800))
        , SvgAttr.y (toString (pixelHeight / 2 - 500))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.opacity "20%"
        , SvgAttr.xlinkHref "./assets/image/Shop.jpg"
        ]
        []


viewShopChoose : Model -> Html Msg
viewShopChoose model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "black"
        ]
        [ viewTask model
        , Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewBuySvg
            , viewTaskBoard
            ]

        -- , healthButton
        -- , damageButton
        , drawButton model
        , exitButton
        , enterUpgradeButton
        , viewBagCoin model
        ]


healthButton : Html Msg
healthButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "400px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "200px"
        , HtmlAttr.style "left" "1000px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "400px"
        , onClick UpgradeHealth
        ]
        [ text "50 coins to upgrade (+5) health of all heroes" ]


exitButton : Html Msg
exitButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "920px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "50px"
        , HtmlAttr.style "left" "1500px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "100px"
        , onClick ExitShop
        ]
        [ text "Exit" ]


damageButton : Html Msg
damageButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "400px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "200px"
        , HtmlAttr.style "left" "500px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "400px"
        , onClick UpgradeDamage
        ]
        [ text "50 coins to upgrade (+2) damage of all heroes" ]


drawButton : Model -> Html Msg
drawButton model =
    if List.length model.indexedheroes >= 6 then
        button
            [ HtmlAttr.style "background" "#34495f"
            , HtmlAttr.style "top" "400px"
            , HtmlAttr.style "color" "white"
            , HtmlAttr.style "font-size" "18px"
            , HtmlAttr.style "font-weight" "500"
            , HtmlAttr.style "height" "200px"
            , HtmlAttr.style "left" "1000px"
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "outline" "none"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "width" "400px"
            ]
            [ text "You have obtained all heroes!" ]

    else
        button
            [ HtmlAttr.style "background" "#34495f"
            , HtmlAttr.style "top" "400px"
            , HtmlAttr.style "color" "white"
            , HtmlAttr.style "font-size" "18px"
            , HtmlAttr.style "font-weight" "500"
            , HtmlAttr.style "height" "200px"
            , HtmlAttr.style "left" "1000px"
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "outline" "none"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "width" "400px"
            , onClick LuckyDraw
            ]
            [ text "100 coins to draw a powerful hero!" ]


viewUpgradePage : Model -> Html Msg
viewUpgradePage model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "black"
        ]
        ([ viewTask model
         , Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            ([ viewBuySvg
             , viewTaskBoard
             ]
                ++ List.map (\hero -> viewShopHeroes model hero) (idealAllHeroes model)
            )
         , exitButton
         , viewBagCoin model
         ]
            ++ upgradeButton model
        )


enterUpgradeButton : Html Msg
enterUpgradeButton =
    button
        [ HtmlAttr.style "background" "#34495f"
        , HtmlAttr.style "top" "400px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-size" "18px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "200px"
        , HtmlAttr.style "left" "500px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "400px"
        , onClick EnterUpgrade
        ]
        [ text "go to upgrade your heroes" ]


upgradeButton : Model -> List (Html Msg)
upgradeButton model =
    let
        selected =
            List.head (List.filter (\( _, index ) -> index == model.upgradePageIndex) (idealAllHeroes model))
    in
    case selected of
        Just ( hero, ind ) ->
            if isClassHave ( hero, ind ) model then
                [ button
                    [ HtmlAttr.style "background" "#34495f"
                    , HtmlAttr.style "top" "750px"
                    , HtmlAttr.style "color" "white"
                    , HtmlAttr.style "font-size" "18px"
                    , HtmlAttr.style "font-weight" "500"
                    , HtmlAttr.style "height" "200px"
                    , HtmlAttr.style "left" "800px"
                    , HtmlAttr.style "line-height" "60px"
                    , HtmlAttr.style "outline" "none"
                    , HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "width" "400px"
                    , onClick (LevelUp ( hero, ind ))
                    ]
                    [ text
                        ("50 coins to upgrade "
                            ++ toString hero.class
                        )
                    ]
                ]

            else
                [ button
                    [ HtmlAttr.style "background" "#34495f"
                    , HtmlAttr.style "top" "750px"
                    , HtmlAttr.style "color" "white"
                    , HtmlAttr.style "font-size" "18px"
                    , HtmlAttr.style "font-weight" "500"
                    , HtmlAttr.style "height" "200px"
                    , HtmlAttr.style "left" "800px"
                    , HtmlAttr.style "line-height" "60px"
                    , HtmlAttr.style "outline" "none"
                    , HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "width" "400px"
                    ]
                    [ text
                        ("You haven't obtained "
                            ++ toString hero.class
                        )
                    ]
                ]

        Nothing ->
            []


viewShopHeroes : Model -> ( Hero, Int ) -> Svg msg
viewShopHeroes model ( hero, index ) =
    let
        y =
            case modBy 6 (index - model.upgradePageIndex) of
                0 ->
                    300

                _ ->
                    400

        x =
            case modBy 6 (index - model.upgradePageIndex) of
                5 ->
                    200

                0 ->
                    800

                1 ->
                    1400

                2 ->
                    2000

                4 ->
                    -400

                _ ->
                    2600

        mywidth =
            case modBy 6 (index - model.upgradePageIndex) of
                0 ->
                    400

                _ ->
                    300

        class =
            toString hero.class
    in
    if isClassHave ( hero, index ) model then
        Svg.image
            [ SvgAttr.width (toString mywidth)
            , SvgAttr.height (toString mywidth)
            , SvgAttr.x (toString x)
            , SvgAttr.y (toString y)
            , SvgAttr.preserveAspectRatio "none"
            , SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Blue.png")
            ]
            []

    else
        Svg.image
            [ SvgAttr.width (toString mywidth)
            , SvgAttr.height (toString mywidth)
            , SvgAttr.x (toString x)
            , SvgAttr.y (toString y)
            , SvgAttr.preserveAspectRatio "none"
            , SvgAttr.xlinkHref "./assets/image/Locked.png"
            ]
            []


viewDrawnHero : Model -> Class -> Html Msg
viewDrawnHero model class =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "background" "black"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewBuySvg
            , Svg.image
                [ SvgAttr.width "400"
                , SvgAttr.height "400"
                , SvgAttr.x "800"
                , SvgAttr.y "400"
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref ("./assets/image/" ++ toString class ++ "Blue.png")
                ]
                []
            ]
        , exitButton
        ]


isClassHave : ( Hero, Int ) -> Model -> Bool
isClassHave ( _, ind ) model =
    List.member ind (List.map (\( _, index ) -> index) model.indexedheroes)


idealAllHeroes : Model -> List ( Hero, Int )
idealAllHeroes model =
    let
        donthave =
            List.filter (\( _, index ) -> not (List.member index (List.map (\( _, yindex ) -> yindex) model.indexedheroes))) Data.allSampleHeroes
    in
    model.indexedheroes ++ donthave
