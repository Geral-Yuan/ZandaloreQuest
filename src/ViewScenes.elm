module ViewScenes exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, audio, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (Msg)
import Model exposing (Model)
import RpgCharacter exposing (RpgCharacter)
import Svg exposing (Svg, text, view)
import Svg.Attributes as SvgAttr
import ViewEncyclopedia exposing (viewEncyclopediaButton)
import ViewNPCTask exposing (viewSingleNPC, viewTask, viewTaskBoard)
import ViewOthers exposing (determineOpct, viewCoinSVG, viewPopUpHint, viewUIButton, viewUIFrame)


logoWidth : Float
logoWidth =
    300 * sqrt 3


logoHeight : Float
logoHeight =
    600


startWidth : Float
startWidth =
    2000


startHeight : Float
startHeight =
    1000


viewScene0 : Model -> Html Msg
viewScene0 model =
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
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewLogo model.time ]
        ]


viewLogo : Float -> Svg Msg
viewLogo t =
    Svg.image
        [ SvgAttr.width (toString logoWidth)
        , SvgAttr.height (toString logoHeight)
        , SvgAttr.x (toString (pixelWidth / 2 - logoWidth / 2))
        , SvgAttr.y (toString (pixelHeight / 2 - logoHeight / 2))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.opacity (determineOpct t 6 |> String.fromFloat)
        , SvgAttr.xlinkHref "./assets/image/logo.png"
        ]
        []


viewRpgCharacter : RpgCharacter -> Html Msg
viewRpgCharacter character =
    let
        ( x, y ) =
            character.pos

        w =
            character.width

        h =
            character.height

        scaleFactor =
            case character.faceDir of
                Left ->
                    -1

                Right ->
                    1

                _ ->
                    0

        image =
            if character.moveLeft || character.moveRight || character.moveUp || character.moveDown then
                "GIF.gif"

            else
                ".png"
    in
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (toString (x - w / 2) ++ "px")
        , HtmlAttr.style "top" (toString (y - h / 2) ++ "px")
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ img
            [ src ("./assets/image/MainCharacter" ++ image)
            , width (floor w)
            , height (floor h)
            , HtmlAttr.style "transform" ("scaleX(" ++ toString scaleFactor ++ ")")
            ]
            []
        ]


viewCharacterPos : RpgCharacter -> Html Msg
viewCharacterPos character =
    let
        ( x, y ) =
            character.pos
    in
    div
        [ HtmlAttr.style "bottom" "100px"
        , HtmlAttr.style "left" "0px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("( " ++ toString (floor x) ++ " ," ++ toString (floor y) ++ " )") ]


viewCastle : Model -> Html Msg
viewCastle model =
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
        , HtmlAttr.style "font-family" "myfont"
        ]
        (viewKeyGif
            ++ [ viewTask model
               , Svg.svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    ]
                    ([ viewCastleSvg
                     , viewCoinSVG ( 1500, 900 )
                     ]
                        ++ viewTaskBoard
                        ++ viewUIFrame 200 42 1525 688
                        -- shop
                        ++ viewUIFrame 200 42 268 688
                        ++ viewUIFrame 200 42 899 270
                        -- dungeon1
                        ++ viewUIButton 170 80 29 800
                    )

               --, viewCharacterPos model.character
               , viewBagCoin model
               , viewTipForDir
               , viewTipForC
               , viewTipForEnter
               , viewEncyclopediaButton
               , viewTipForT
               , viewDungeon1Location
               , viewShopLocation
               , viewDungeon2Location
               , div
                    [ HtmlAttr.style "bottom" "20px"
                    , HtmlAttr.style "left" "0px"
                    , HtmlAttr.style "position" "absolute"
                    ]
                    [ audio
                        [ HtmlAttr.autoplay True
                        , HtmlAttr.loop True
                        , HtmlAttr.preload "True"
                        , HtmlAttr.controls True
                        , HtmlAttr.src "./assets/audio/CastleThemeSong.mp3"
                        , HtmlAttr.id "CastleThemeSong"
                        ]
                        []
                    ]
               ]
            ++ List.concat (List.map viewSingleNPC (model.npclist |> List.filter (\x -> x.scene == CastleScene)))
            ++ (viewRpgCharacter model.character
                    :: viewPopUpHint model
               )
        )


viewDungeon : Model -> Html Msg
viewDungeon model =
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
        , HtmlAttr.style "font-family" "myfont"
        ]
        (viewKeyGif
            ++ [ viewTask model
               , Svg.svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    ]
                    ([ viewDungeonSvg
                     , viewCoinSVG ( 1500, 900 )
                     ]
                        ++ viewTaskBoard
                        ++ viewUIButton 170 80 29 800
                    )

               --, viewCharacterPos model.character
               , viewBagCoin model
               , viewTipForT
               , viewTipForDir
               , viewTipForC
               , viewTipForEnter
               , viewEncyclopediaButton
               ]
            ++ List.concat (List.map viewSingleNPC (model.npclist |> List.filter (\x -> x.scene == DungeonScene)))
            ++ [ viewRpgCharacter model.character ]
        )


viewDungeon2 : Model -> Html Msg
viewDungeon2 model =
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
        , HtmlAttr.style "font-family" "myfont"
        ]
        (viewKeyGif
            ++ [ viewTask model
               , Svg.svg
                    [ SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    ]
                    ([ viewDungeonSvg
                     , viewCoinSVG ( 1500, 900 )
                     ]
                        ++ viewTaskBoard
                        ++ viewUIButton 170 80 29 800
                    )

               --, viewCharacterPos model.character
               , viewBagCoin model
               , viewTipForDir
               , viewTipForT
               , viewTipForC
               , viewTipForEnter
               , viewEncyclopediaButton
               ]
            ++ List.concat (List.map viewSingleNPC (model.npclist |> List.filter (\x -> x.scene == Dungeon2Scene)))
            ++ [ viewRpgCharacter model.character ]
        )



-- viewExit : Svg Msg
-- viewExit =
--     Svg.rect
--         [ SvgAttr.width "60"
--         , SvgAttr.height "90"
--         , SvgAttr.x (toString (pixelWidth / 2 - 30))
--         , SvgAttr.y (toString (pixelHeight / 2 + 410))
--         , SvgAttr.fontSize "20"
--         , SvgAttr.fontStyle "blue"
--         , SvgAttr.color "black"
--         , SvgAttr.fill "white"
--         ]
--         [ Svg.text "Exit" ]


viewDungeonSvg : Svg Msg
viewDungeonSvg =
    Svg.image
        [ SvgAttr.width "1600"
        , SvgAttr.height "1000"
        , SvgAttr.x (toString (pixelWidth / 2 - 800))
        , SvgAttr.y (toString (pixelHeight / 2 - 500))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Dungeon.jpg"
        ]
        []


viewKeyGif : List (Html msg)
viewKeyGif =
    [ viewSvgForDir
    , viewSvgForC
    , viewSvgForEnter
    ]


viewSvgForDir : Html msg
viewSvgForDir =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "top" "100px"
        , HtmlAttr.style "left" "0px"
        ]
        [ img [ src "./assets/image/keyGIF.gif", height 150, width 225 ] []
        ]


viewSvgForC : Svg msg
viewSvgForC =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "top" "600px"
        , HtmlAttr.style "left" "72.5px"
        ]
        [ img [ src "./assets/image/cGIF.gif", height 80, width 80 ] []
        ]


viewSvgForEnter : Svg msg
viewSvgForEnter =
    div
        [ HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "top" "400px"
        , HtmlAttr.style "left" "32.5px"
        ]
        [ img [ src "./assets/image/enterGIF.gif", height 80, width 160 ] []
        ]


viewShopLocation : Html msg
viewShopLocation =
    div
        [ HtmlAttr.style "top" "680px"
        , HtmlAttr.style "left" "1599px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "27px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Shop" ]


viewDungeon1Location : Html msg
viewDungeon1Location =
    div
        [ HtmlAttr.style "top" "263px"
        , HtmlAttr.style "left" "921px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "27px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Main Dungeon" ]


viewDungeon2Location : Html msg
viewDungeon2Location =
    div
        [ HtmlAttr.style "top" "682px"
        , HtmlAttr.style "left" "286px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "30px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Side Dungeon" ]


viewTipForDir : Html Msg
viewTipForDir =
    div
        [ HtmlAttr.style "top" "280px"
        , HtmlAttr.style "left" "10px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "22px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Press ⬆⬅⬇➡ to move" ]


viewTipForT : Html Msg
viewTipForT =
    div
        [ HtmlAttr.style "top" "30px"
        , HtmlAttr.style "left" "0px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "22px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Press T to access all levels" ]


viewTipForC : Html Msg
viewTipForC =
    div
        [ HtmlAttr.style "top" "700px"
        , HtmlAttr.style "left" "5px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "22px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Press C to talk to NPCs" ]


viewTipForEnter : Html Msg
viewTipForEnter =
    div
        [ HtmlAttr.style "top" "500px"
        , HtmlAttr.style "left" "-5px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "22px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Press Enter to pass doors" ]


viewCastleSvg : Svg msg
viewCastleSvg =
    Svg.image
        [ SvgAttr.width "1600"
        , SvgAttr.height "1000"
        , SvgAttr.x (toString (pixelWidth / 2 - 800))
        , SvgAttr.y (toString (pixelHeight / 2 - 500))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/Castle.png"
        ]
        []


viewStarting : Model -> Html Msg
viewStarting model =
    -- Add this when the homepage has been designed
    let
        ( w, h ) =
            model.size

        r =
            if w / h > startWidth / startHeight then
                Basics.min 1 (h / startHeight)

            else
                Basics.min 1 (w / startWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat startWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat startHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat (600 + (w - startWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - startHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , ("url('./assets/image/Start.png')"
            ++ " no-repeat fixed "
            ++ " 0px "
            ++ " 0px / "
            ++ (toString startWidth ++ "px " ++ (toString startHeight ++ "px"))
          )
            |> HtmlAttr.style "background"
        ]
        []


viewBagCoin : Model -> Html Msg
viewBagCoin model =
    div
        [ HtmlAttr.style "left" "1600px"
        , HtmlAttr.style "top" "910px"
        , HtmlAttr.style "color" "orange"
        , HtmlAttr.style "font-family" "myfont"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text (toString model.bag.coins) ]


viewSummary : Model -> Html Msg
viewSummary model =
    let
        ( w, h ) =
            model.size

        r =
            if w / h > startWidth / startHeight then
                Basics.min 1 (h / startHeight)

            else
                Basics.min 1 (w / startWidth)
    in
    div
        [ HtmlAttr.style "width" (String.fromFloat startWidth ++ "px")
        , HtmlAttr.style "height" (String.fromFloat startHeight ++ "px")
        , HtmlAttr.style "position" "absolute"
        , HtmlAttr.style "left" (String.fromFloat ((w - startWidth * r) / 2) ++ "px")
        , HtmlAttr.style "top" (String.fromFloat ((h - startHeight * r) / 2) ++ "px")
        , HtmlAttr.style "transform-origin" "0 0"
        , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
        , HtmlAttr.style "font-family" "myfont"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (viewBoardGameBackGround 20
                :: viewUIFrame 800 900 600 50
            )
        , div
            [ HtmlAttr.style "top" "200px"
            , HtmlAttr.style "left" "0px"
            , HtmlAttr.style "width" "2000px"
            , HtmlAttr.style "color" "white"
            , HtmlAttr.style "font-family" "myfont"
            , HtmlAttr.style "font-size" "40px"
            , HtmlAttr.style "font-weight" "bold"
            , HtmlAttr.style "text-align" "center" -- align text seems to not working
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "position" "absolute"
            ]
            [ text "Great Battle!" ]
        , div
            [ HtmlAttr.style "top" "350px"
            , HtmlAttr.style "left" "0px"
            , HtmlAttr.style "width" "2000px"
            , HtmlAttr.style "color" "white"
            , HtmlAttr.style "font-family" "myfont"
            , HtmlAttr.style "font-size" "40px"
            , HtmlAttr.style "font-weight" "bold"
            , HtmlAttr.style "text-align" "center"
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "position" "absolute"
            ]
            [ text ("Gold: +" ++ toString (model.board.coins + 50)) ]
        , div
            [ HtmlAttr.style "top" "750px"
            , HtmlAttr.style "left" "0px"
            , HtmlAttr.style "width" "2000px"
            , HtmlAttr.style "color" "white"
            , HtmlAttr.style "font-family" "myfont"
            , HtmlAttr.style "font-size" "40px"
            , HtmlAttr.style "font-weight" "bold"
            , HtmlAttr.style "text-align" "center"
            , HtmlAttr.style "line-height" "60px"
            , HtmlAttr.style "position" "absolute"
            ]
            [ text "Click Anywhere To Continue" ]
        ]


viewBoardGameBackGround : Int -> Svg Msg
viewBoardGameBackGround opac =
    Svg.image
        [ SvgAttr.width "2000"
        , SvgAttr.height "1000"
        , SvgAttr.x "0"
        , SvgAttr.y (toString (pixelHeight / 2 - 500))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.opacity (toString opac ++ "%")
        , SvgAttr.xlinkHref "./assets/image/BoardGameBG.png"
        ]
        []
