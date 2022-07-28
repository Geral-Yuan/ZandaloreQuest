module ViewShop exposing (viewDrawnHero, viewShop, viewShopChoose, viewUpgradePage)

{-| This file fills pages in the shop.

# Functions 
@docs viewDrawnHero, viewShop, viewShopChoose, viewUpgradePage

-}

import Data exposing (Class(..), Hero, HeroState(..), Scene(..), pixelHeight, pixelWidth)
import Debug exposing (toString)
import Html exposing (Html, button, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Model exposing (Model)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr
import ViewNPCTask exposing (viewSingleNPC, viewTask, viewTaskBoard)
import ViewOthers exposing (viewCoinSVG, viewUIButton, viewUIFrame)
import ViewScenes exposing (viewKeyGif, viewTipForDir, viewCharacterPos, viewTipForC, viewTipForEnter, viewBagCoin, viewRpgCharacter)
import Data exposing (upgradeHealth)
import Data exposing (upgradeDamage)

{-| view the shop where the rpg character can move -}
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
                    (
                        
                        viewShopSvg
                        ++ viewTaskBoard
                        ++ [viewCoinSVG ( 1500, 900 )]
                    )
               --, viewCharacterPos model.character
               , viewTipForDir
               , viewTipForC
               , viewTipForEnter
               , viewBagCoin model
               ]
            ++ List.concat (List.map viewSingleNPC (model.npclist |> List.filter (\x -> x.scene == ShopScene)))
            ++ [ viewRpgCharacter model.character ]
        )


viewShopSvg : List (Svg msg)
viewShopSvg =
    [
        Svg.image
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
    [
    -- Svg.linearGradient
    --     [ SvgAttr.id ("myLRadial")
    --     , SvgAttr.x1 "0%"
    --     , SvgAttr.x2 "100%"
    --     , SvgAttr.y1 "0%"
    --     , SvgAttr.y2 "0%" ]
    --     [ Svg.stop
    --         [ SvgAttr.offset "0%"
    --         , SvgAttr.stopOpacity "100%"
    --         , SvgAttr.stopColor "transparent"
    --         ]
    --         []
    --     -- , Svg.stop
    --     --     [ SvgAttr.offset "50%"
    --     --     , SvgAttr.stopOpacity "0%"
    --     --     ]
    --     --     []
    --     , Svg.stop
    --         [ SvgAttr.offset "100%"
    --         , SvgAttr.stopOpacity "0%"
    --         , SvgAttr.stopColor "transparent"
    --         ]
    --         []
    --     ]
    
    -- , 
    Svg.image
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
        [ SvgAttr.id ("myRadial") ]
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
    -- , Svg.circle
    --     [ SvgAttr.cx "1000"
    --     , SvgAttr.cy "400"
    --     , SvgAttr.r "250"
    --     , SvgAttr.fill "url('#myRadial')"
    --     ]
    --     []
    ]
    

{-| view the first page where players can draw a hero and enter upgrade page -}
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
            (
                (viewBuySvg ++ viewTaskBoard)
                ++ (viewUIButton 100 50 1400 920) --for exit
                ++ (viewUIButton 400 200 1000 400) -- for draw
                ++ (viewUIButton 400 200 500 400) -- for upgrade
                ++ [viewCoinSVG ( 1500, 900 )]
            )
        , drawButton model
        , exitButton
        , enterUpgradeButton
        , viewBagCoin model
        ]

exitButton : Html Msg
exitButton =
    button
        [ HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "border" "transparent"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "18px"

        , HtmlAttr.style "top" "920px"
        , HtmlAttr.style "height" "50px"
        , HtmlAttr.style "left" "1400px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "100px"
        , onClick ExitShop
        ]
        [ text "Exit" ]


drawButton : Model -> Html Msg
drawButton model =
    if List.length model.indexedheroes >= 6 then
        button
            [ HtmlAttr.style "background" "#34495f"
            , HtmlAttr.style "top" "400px"
            , HtmlAttr.style "color" "white"
            , HtmlAttr.style "font-size" "24px"
            , HtmlAttr.style "font-weight" "500"
            , HtmlAttr.style "height" "200px"
            , HtmlAttr.style "left" "1000px"
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "outline" "none"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "width" "400px"
            , HtmlAttr.style "background" "transparent"
            , HtmlAttr.style "border" "transparent"
            , HtmlAttr.style "font-weight" "bold"
            , HtmlAttr.style "color" "rgb(61,43,31)"
            , HtmlAttr.style "font-size" "24px"
            ]
            [ text "You have obtained all heroes!" ]

    else if List.length model.indexedheroes <= 3 then
        button
            [ HtmlAttr.style "background" "#34495f"
            , HtmlAttr.style "top" "400px"
            , HtmlAttr.style "color" "white"
            , HtmlAttr.style "font-size" "24px"
            , HtmlAttr.style "font-weight" "500"
            , HtmlAttr.style "height" "200px"
            , HtmlAttr.style "left" "1000px"
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "outline" "none"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "width" "400px"
            , HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "border" "transparent"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "24px"
            , onClick LuckyDraw
            ]
            [ text "0 coins to draw a powerful hero!" ]

    else
        button
            [ HtmlAttr.style "background" "#34495f"
            , HtmlAttr.style "top" "400px"
            , HtmlAttr.style "color" "white"
            , HtmlAttr.style "font-size" "24px"
            , HtmlAttr.style "font-weight" "500"
            , HtmlAttr.style "height" "200px"
            , HtmlAttr.style "left" "1000px"
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "outline" "none"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "width" "400px"
            , HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "border" "transparent"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "24px"
            , onClick LuckyDraw
            ]
            [ text "100 coins to draw a powerful hero!" ]

{-| view the page where the player can upgrade heroes -}
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
            (   viewBuySvg
                
                ++ viewTaskBoard
                ++ viewHighlightHero
                ++ List.map (\hero -> viewShopHeroes model hero) (idealAllHeroes model)
                ++ viewHeroAttr model
                ++ (viewUIButton 100 50 1400 920) --for exit
            ++ (viewUIButton 300 150 850 775) -- for upgrade
            ++ [viewCoinSVG ( 1500, 900 )]
            
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
        , HtmlAttr.style "font-size" "24px"
        , HtmlAttr.style "font-weight" "500"
        , HtmlAttr.style "height" "200px"
        , HtmlAttr.style "left" "500px"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "outline" "none"
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "width" "400px"
        , HtmlAttr.style "background" "transparent"
        , HtmlAttr.style "border" "transparent"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "color" "rgb(61,43,31)"
        , HtmlAttr.style "font-size" "24px"
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
                    [ HtmlAttr.style "top" "775px"
                    , HtmlAttr.style "font-size" "24px"
                    , HtmlAttr.style "height" "150px"
                    , HtmlAttr.style "left" "850px"
                    , HtmlAttr.style "line-height" "60px"
                    , HtmlAttr.style "outline" "none"
                    , HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "width" "300px"
                    , HtmlAttr.style "background" "transparent"
                    , HtmlAttr.style "border" "transparent"
                    , HtmlAttr.style "font-weight" "bold"
                    , HtmlAttr.style "color" "rgb(61,43,31)"
                    , onClick (LevelUp ( hero, ind ))
                    , Html.Events.onMouseOver (DisplayUpgrade True)
                    , Html.Events.onMouseOut (DisplayUpgrade False)
                    ]
                    [ text
                        ("50 coins to upgrade "
                            ++ toString hero.class
                        )
                    ]
                ]

            else
                [ button
                    [ HtmlAttr.style "top" "750px"
                    , HtmlAttr.style "height" "200px"
                    , HtmlAttr.style "left" "800px"
                    , HtmlAttr.style "line-height" "60px"
                    , HtmlAttr.style "outline" "none"
                    , HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "width" "400px"
                    , HtmlAttr.style "background" "transparent"
                    , HtmlAttr.style "border" "transparent"
                    , HtmlAttr.style "font-weight" "bold"
                    , HtmlAttr.style "color" "rgb(61,43,31)"
                    , HtmlAttr.style "font-size" "24px"
                    ]
                    [ text
                        ("Locked "
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


{-| view the hero that the player has just drawn -}
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
            (viewBuySvg
            ++   (Svg.image
                    [ SvgAttr.width "400"
                    , SvgAttr.height "400"
                    , SvgAttr.x "800"
                    , SvgAttr.y "400"
                    , SvgAttr.preserveAspectRatio "none"
                    , SvgAttr.xlinkHref ("./assets/image/" ++ toString class ++ "Blue.png")
                    ]
                    [])
                 
            :: (viewUIButton 100 50 1400 920)) --for exit
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


viewHeroAttr : Model -> List (Svg Msg)
viewHeroAttr model =
    let
        maybeChosen = List.filter (\x -> isClassHave x model) model.indexedheroes
                      |> List.filter (\(_, index) -> index == model.upgradePageIndex ) 
                      |> List.head

        shealth =
            case maybeChosen of
                Nothing ->
                    "???"

                Just (hero, _) ->
                    if isdplup then
                        toString (hero.maxHealth) ++ " + " ++ toString (upgradeHealth hero.class)
                    else
                        toString (hero.maxHealth)

        sdmg =
            case maybeChosen of
                Nothing ->
                    "???"

                Just (hero, _) ->
                    if isdplup then
                        toString (hero.damage) ++ " + " ++ toString (upgradeDamage hero.class)
                    else
                        toString (hero.damage)
        senergy =
            case maybeChosen of
                Nothing ->
                    "???"

                Just (hero, _) ->
                    toString (hero.energy)

        isdplup = model.isDisplayUpgrade
    in
    viewUIFrame  400 240 800 20
    ++ [ Svg.image
        [ SvgAttr.width "55"
        , SvgAttr.height "55"
        , SvgAttr.x "850"
        , SvgAttr.y "40"
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Heart.png"
        ]
        []
    , Svg.image
        [ SvgAttr.width "55"
        , SvgAttr.height "55"
        , SvgAttr.x "850"
        , SvgAttr.y "110"
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Sword.png"
        ]
        []
    , Svg.image
        [ SvgAttr.width "55"
        , SvgAttr.height "55"
        , SvgAttr.x "850"
        , SvgAttr.y "180"
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Energy.png"
        ]
        []
    , Svg.text_
        [ SvgAttr.x "950"
        , SvgAttr.y "67.5"
        , SvgAttr.dominantBaseline "middle"
        , SvgAttr.fill "white"
        , SvgAttr.fontSize "50"
        ]
        [ Svg.text shealth
        ]
    , Svg.text_
        [ SvgAttr.x "950"
        , SvgAttr.y "137.5"
        , SvgAttr.dominantBaseline "middle"
        , SvgAttr.fill "white"
        , SvgAttr.fontSize "50"
        ]
        [ Svg.text sdmg
        ]
    , Svg.text_
        [ SvgAttr.x "950"
        , SvgAttr.y "208"
        , SvgAttr.dominantBaseline "middle"
        , SvgAttr.fill "white"
        , SvgAttr.fontSize "50"
        ]
        [ Svg.text senergy
        ]
    
    ]
