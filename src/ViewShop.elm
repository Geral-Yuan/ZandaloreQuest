module ViewShop exposing (viewDrawnHero, viewShop, viewShopChoose, viewUpgradePage)

{-| This file fills pages in the shop.


# Functions

@docs viewDrawnHero, viewShop, viewShopChoose, viewUpgradePage

-}

import Data exposing (upgradeDamage, upgradeHealth)
import Debug exposing (toString)
import Html exposing (Attribute, Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import Type exposing (Class(..), Hero, HeroState(..), Model, Scene(..))
import ViewConst exposing (pixelHeight, pixelWidth)
import ViewNPCTask exposing (viewSingleNPC, viewTask, viewTaskBoard)
import ViewOthers exposing (viewCoinSVG, viewUIButton, viewUIFrame)
import ViewScenes exposing (viewBagCoin, viewKeyGif, viewRpgCharacter, viewTipForKeys)
import ViewEncyclopedia exposing (viewEncyclopediaButton)


{-| view the shop where the rpg character can move
-}
viewShop : Model -> Html Msg
viewShop model =
    div
        (shopDivStyle model)
        (viewKeyGif
            ++ [ viewTask model
               , Svg.svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    ]
                    (viewShopSvg
                        ++ viewTaskBoard
                        ++ viewUIButton 170 80 10 650
                        ++ [ viewCoinSVG ( 1500, 900 ) ]
                    )

               --, viewCharacterPos model.character
               , viewEncyclopediaButton
               , viewBagCoin model
               ]
            ++ viewTipForKeys
            ++ List.concat (List.map viewSingleNPC (model.npclist |> List.filter (\x -> x.scene == ShopScene)))
            ++ [ viewRpgCharacter model.character ]
        )


viewShopSvg : List (Svg msg)
viewShopSvg =
    [ Svg.image
        [ SvgAttr.width "1600"
        , SvgAttr.height "1000"
        , SvgAttr.x (toString (pixelWidth / 2 - 800))
        , SvgAttr.y (toString (pixelHeight / 2 - 500))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Shop.jpg"
        ]
        []
    ]


viewBuySvg : List (Svg msg)
viewBuySvg =
    [ Svg.image
        [ SvgAttr.width "1600"
        , SvgAttr.height "1000"
        , SvgAttr.x (toString (pixelWidth / 2 - 800))
        , SvgAttr.y (toString (pixelHeight / 2 - 500))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.opacity "40%" --"url('#myLRadial')"
        , SvgAttr.xlinkHref "./assets/image/Shop.jpg"
        ]
        []
    ]


viewHighlightHero : List (Svg msg)
viewHighlightHero =
    [ Svg.radialGradient
        [ SvgAttr.id "myRadial" ]
        [ Svg.stop
            [ SvgAttr.offset "10%"
            , SvgAttr.stopColor "white"
            , SvgAttr.stopOpacity "100%"
            ]
            []
        , Svg.stop
            [ SvgAttr.offset "100%"
            , SvgAttr.stopColor "white"
            , SvgAttr.stopOpacity "0%"
            ]
            []
        ]
    , Svg.ellipse
        [ SvgAttr.cx "1000"
        , SvgAttr.cy "700"
        , SvgAttr.rx "250"
        , SvgAttr.ry "50"
        , SvgAttr.fill "url('#myRadial')"
        ]
        []
    , Svg.rect
        [ SvgAttr.x "750"
        , SvgAttr.y "270"
        , SvgAttr.width "500"
        , SvgAttr.height "380"
        , SvgAttr.rx "50"
        , SvgAttr.fill "url('#myRadial')"
        ]
        []
    ]


{-| view the first page where players can draw a hero and enter upgrade page
-}
viewShopChoose : Model -> Html Msg
viewShopChoose model =
    div
        (shopDivStyle model)
        [ viewTask model
        , Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            ((viewBuySvg ++ viewTaskBoard)
                ++ viewUIButton 100 50 1400 920
                --for exit
                ++ viewUIButton 400 200 1000 400
                -- for draw
                ++ viewUIButton 400 200 500 400
                -- for upgrade
                ++ [ viewCoinSVG ( 1500, 900 ) ]
            )
        , drawButton model
        , exitButton
        , enterUpgradeButton
        , viewBagCoin model
        ]


exitButton : Html Msg
exitButton =
    button
        (shopButtonStyle
            ++ [ HtmlAttr.style "font-size" "18px"
               , HtmlAttr.style "top" "920px"
               , HtmlAttr.style "height" "50px"
               , HtmlAttr.style "left" "1400px"
               , HtmlAttr.style "line-height" "60px"
               , HtmlAttr.style "width" "100px"
               , onClick ExitShop
               ]
        )
        [ text "Exit" ]


drawButton : Model -> Html Msg
drawButton model =
    let
        display =
            if List.length model.indexedheroes >= 6 then
                "You have obtained all heroes!"

            else if List.length model.indexedheroes <= 2 then
                "0 coins to recruit a random hero!"

            else
                "100 coins to recruit a random hero!"

        button_style =
            shopButtonStyle
                    ++ [ HtmlAttr.style "top" "400px"
                    , HtmlAttr.style "height" "200px"
                    , HtmlAttr.style "left" "1000px"
                    , HtmlAttr.style "line-height" "60px"
                    , HtmlAttr.style "width" "400px"
                    , HtmlAttr.style "font-size" "24px"
                    ]

        draw_style = 
            if List.length model.indexedheroes >=6 then
                button_style
            else
                (onClick LuckyDraw) :: button_style
    in
    button
        draw_style
        [ text display ]


{-| view the page where the player can upgrade heroes
-}
viewUpgradePage : Model -> Html Msg
viewUpgradePage model =
    div
        (shopDivStyle model)
        ([ viewTask model
         , Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewBuySvg
                ++ viewTaskBoard
                ++ viewHighlightHero
                ++ List.map (\hero -> viewShopHeroes model hero) (idealAllHeroes model)
                ++ viewHeroAttr model
                ++ viewUIButton 100 50 1400 920
                --for exit
                ++ viewUIButton 300 150 850 775
                -- for upgrade
                ++ [ viewCoinSVG ( 1500, 900 ) ]
            )
         , exitButton
         , viewBagCoin model
         ]
            ++ upgradeButton model
        )


enterUpgradeButton : Html Msg
enterUpgradeButton =
    button
        (shopButtonStyle
            ++ [ HtmlAttr.style "top" "400px"
               , HtmlAttr.style "font-size" "24px"
               , HtmlAttr.style "height" "200px"
               , HtmlAttr.style "left" "500px"
               , HtmlAttr.style "line-height" "60px"
               , HtmlAttr.style "width" "400px"
               , HtmlAttr.style "font-size" "24px"
               , onClick EnterUpgrade
               ]
        )
        [ text
            "Upgrade your heroes"
        ]


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
                    (shopButtonStyle
                        ++ [ HtmlAttr.style "top" "775px"
                           , HtmlAttr.style "font-size" "24px"
                           , HtmlAttr.style "height" "150px"
                           , HtmlAttr.style "left" "850px"
                           , HtmlAttr.style "line-height" "60px"
                           , HtmlAttr.style "width" "300px"
                           , onClick (LevelUp ( hero, ind ))
                           , Html.Events.onMouseOver (DisplayUpgrade True)
                           , Html.Events.onMouseOut (DisplayUpgrade False)
                           ]
                    )
                    [ text
                        ("50 coins to upgrade " ++ toString hero.class)
                    ]
                ]

            else
                [ button
                    (shopButtonStyle
                        ++ [ HtmlAttr.style "top" "750px"
                           , HtmlAttr.style "height" "200px"
                           , HtmlAttr.style "left" "800px"
                           , HtmlAttr.style "line-height" "60px"
                           , HtmlAttr.style "width" "400px"
                           , HtmlAttr.style "font-size" "24px"
                           ]
                    )
                    [ text
                        ("Locked " ++ toString hero.class)
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

        imageStyle =
            [ SvgAttr.width (toString mywidth)
            , SvgAttr.height (toString mywidth)
            , SvgAttr.x (toString x)
            , SvgAttr.y (toString y)
            , SvgAttr.preserveAspectRatio "none"
            ]
    in
    if isClassHave ( hero, index ) model then
        Svg.image
            (SvgAttr.xlinkHref ("./assets/image/" ++ class ++ "Blue.png") :: imageStyle)
            []

    else
        Svg.image
            (SvgAttr.xlinkHref "./assets/image/Locked.png" :: imageStyle)
            []


{-| view the hero that the player has just drawn
-}
viewDrawnHero : Model -> Class -> Html Msg
viewDrawnHero model class =
    div
        (shopDivStyle model)
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewBuySvg
                ++ Svg.image
                    [ SvgAttr.width "400"
                    , SvgAttr.height "400"
                    , SvgAttr.x "800"
                    , SvgAttr.y "400"
                    , SvgAttr.preserveAspectRatio "none"
                    , SvgAttr.xlinkHref ("./assets/image/" ++ toString class ++ "Blue.png")
                    ]
                    []
                :: viewUIButton 100 50 1400 920
            )

        --for exit
        , exitButton
        ]


isClassHave : ( Hero, Int ) -> Model -> Bool
isClassHave ( _, ind ) model =
    List.member ind (List.map (\( _, index ) -> index) model.indexedheroes)


idealAllHeroes : Model -> List ( Hero, Int )
idealAllHeroes model =
    let
        haveIndex =
            List.map (\( _, yindex ) -> yindex) model.indexedheroes

        ishave =
            \( _, index ) -> not (List.member index haveIndex)

        donthave =
            Data.allSampleHeroes
                |> List.filter ishave
    in
    model.indexedheroes ++ donthave


viewHeroAttr : Model -> List (Svg Msg)
viewHeroAttr model =
    let
        maybeChosen =
            List.filter (\x -> isClassHave x model) model.indexedheroes
                |> List.filter (\( _, index ) -> index == model.upgradePageIndex)
                |> List.head

        shealth =
            case maybeChosen of
                Nothing ->
                    "???"

                Just ( hero, _ ) ->
                    if isdplup then
                        toString hero.maxHealth ++ " + " ++ toString (upgradeHealth hero.class)

                    else
                        toString hero.maxHealth

        sdmg =
            case maybeChosen of
                Nothing ->
                    "???"

                Just ( hero, _ ) ->
                    if isdplup then
                        toString hero.damage ++ " + " ++ toString (upgradeDamage hero.class)

                    else
                        toString hero.damage

        senergy =
            case maybeChosen of
                Nothing ->
                    "???"

                Just ( hero, _ ) ->
                    toString hero.energy

        isdplup =
            model.isDisplayUpgrade

        imageStyle =
            [ SvgAttr.width "55"
            , SvgAttr.height "55"
            , SvgAttr.x "850"
            , SvgAttr.preserveAspectRatio "none"
            ]

        textStyle =
            [ SvgAttr.x "950"
            , SvgAttr.dominantBaseline "middle"
            , SvgAttr.fill "white"
            , SvgAttr.fontSize "50"
            ]
    in
    viewUIFrame 400 240 800 20
        ++ [ Svg.image
                ([ SvgAttr.y "40", SvgAttr.xlinkHref "./assets/image/Heart.png" ] ++ imageStyle)
                []
           , Svg.image
                ([ SvgAttr.y "110", SvgAttr.xlinkHref "./assets/image/Sword.png" ] ++ imageStyle)
                []
           , Svg.image
                ([ SvgAttr.y "180", SvgAttr.xlinkHref "./assets/image/Energy.png" ] ++ imageStyle)
                []
           , Svg.text_
                (SvgAttr.y "67.5" :: textStyle)
                [ Svg.text shealth
                ]
           , Svg.text_
                (SvgAttr.y "137.5" :: textStyle)
                [ Svg.text sdmg
                ]
           , Svg.text_
                (SvgAttr.y "208" :: textStyle)
                [ Svg.text senergy
                ]
           ]


shopButtonStyle : List (Attribute msg)
shopButtonStyle =
    [ HtmlAttr.style "background" "transparent"
    , HtmlAttr.style "border" "transparent"
    , HtmlAttr.style "font-weight" "bold"
    , HtmlAttr.style "color" "rgb(61,43,31)"
    , HtmlAttr.style "font-family" "myfont"
    , HtmlAttr.style "outline" "none"
    , HtmlAttr.style "position" "absolute"
    ]


shopDivStyle : Model -> List (Attribute msg)
shopDivStyle model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
    in
    [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
    , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
    , HtmlAttr.style "position" "absolute"
    , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
    , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
    , HtmlAttr.style "transform-origin" "0 0"
    , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
    , HtmlAttr.style "font-family" "myfont"
    , HtmlAttr.style "background" "black"
    ]
