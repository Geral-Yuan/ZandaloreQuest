module ViewScenes exposing (..)

import Data exposing (..)
import Debug exposing (toString)
import Html exposing (Html, div, img)
import Html.Attributes as HtmlAttr exposing (height, src, width)
import Message exposing (..)
import Model exposing (Model)
import RpgCharacter exposing (RpgCharacter)
import Svg exposing (Svg, text)
import Svg.Attributes as SvgAttr


logoWidth : Float
logoWidth =
    300 * sqrt 3


logoHeight : Float
logoHeight =
    600


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
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewLogo ]
        ]


determineOpct : Float -> Float
determineOpct t =
    if t <= 6 then
        sin (t * pi / 6) * 2 / 1.732

    else
        0


viewLogo : Svg Msg
viewLogo =
    Svg.image
        [ SvgAttr.width (toString logoWidth)
        , SvgAttr.height (toString logoHeight)
        , SvgAttr.x (toString (pixelWidth / 2 - logoWidth / 2))
        , SvgAttr.y (toString (pixelHeight / 2 - logoHeight / 2))
        , SvgAttr.preserveAspectRatio "none"
        , SvgAttr.xlinkHref "./assets/image/logo.png"
        ]
        []



-- {-
--    viewScene0 : Model -> Html Msg
--    viewScene0 model =
--        let
--            ( w, h ) =
--                model.size
--            -- t =
--            --     model.time
--            r =
--                if w / h > logoWidth / logoHeight then
--                    Basics.min 1 (h / logoHeight)
--                else
--                    Basics.min 1 (w / logoWidth)
--        in
--        div
--            [ HtmlAttr.style "width" (String.fromFloat (logoWidth + 400) ++ "px")
--            , HtmlAttr.style "height" (String.fromFloat (logoHeight + 400) ++ "px")
--            , HtmlAttr.style "position" "absolute"
--            , HtmlAttr.style "left" (String.fromFloat (550 + (w - logoWidth * r) / 2) ++ "px")
--            , HtmlAttr.style "top" (String.fromFloat ((h - logoHeight * r) / 2) ++ "px")
--            -- , HtmlAttr.style "opacity" (determineOpct t |> String.fromFloat)
--            , HtmlAttr.style "transform-origin" "0 0"
--            , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
--            , ("url('./assets/image/logo.png')" ++ " no-repeat fixed " ++ " 0px " ++ " 0px / " ++ " 523.2558px " ++ " 600px")
--                |> HtmlAttr.style "background"
--            ]
--            []
--
-- Created this code so that the RPG character can change facing but there are bugs
-- viewRpgCharacter : Model -> Html Msg
-- viewRpgCharacter model =
--     div
--         [ style "position" "absolute"
--         , style "top" (toString (Tuple.first model.character.pos))
--         , style "left" (toString (Tuple.first model.character.pos))
--         , style "z-index" "9999"
--         ]
--         [ case model.character.faceDir of
--             Left ->
--                 img
--                     [ src "./assets/image/WarriorGood.png"
--                     , height (floor model.character.height)
--                     , width (floor model.character.width)
--                     , style "transform" "scaleX(-1)"
--                     ]
--                     []
--             Right ->
--                 img
--                     [ src "./assets/image/WarriorGood.png"
--                     , height (floor model.character.height)
--                     , width (floor model.character.width)
--                     ]
--                     []
--             _ ->
--                 img
--                     [ src "./assets/image/WarriorGood.png"
--                     , height (floor model.character.height)
--                     , width (floor model.character.width)
--                     ]
--                     []
--         ]


viewRpgCharacter : RpgCharacter -> Svg msg
viewRpgCharacter character =
    let
        ( w, h ) =
            ( character.width, character.height )

        ( x, y ) =
            character.pos
    in
    Svg.image
        [ SvgAttr.width (toString w)
        , SvgAttr.height (toString h)
        , SvgAttr.x (toString (x - w / 2))
        , SvgAttr.y (toString (y - h / 2))
        , SvgAttr.preserveAspectRatio "xMidYMid slice"
        , SvgAttr.xlinkHref "./assets/image/MainCharacter.png"
        ]
        []


viewCharacterPos : RpgCharacter -> Html Msg
viewCharacterPos character =
    let
        ( x, y ) =
            character.pos
    in
    div
        [ HtmlAttr.style "bottom" "30px"
        , HtmlAttr.style "left" "0px"
        , HtmlAttr.style "color" "red"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("( " ++ toString x ++ " ," ++ toString y ++ " )") ]


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
        ]
        [ div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "100px"
            , HtmlAttr.style "left" "0px"
            ]
            [ img [ src "./assets/image/keyGIF.gif", height 150, width 225 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "400px"
            , HtmlAttr.style "left" "32.5px"
            ]
            [ img [ src "./assets/image/enterGIF.gif", height 80, width 160 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "600px"
            , HtmlAttr.style "left" "72.5px"
            ]
            [ img [ src "./assets/image/cGIF.gif", height 80, width 80 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" (toString (pixelHeight / 2 - 250) ++ "px")
            , HtmlAttr.style "left" (toString (pixelWidth / 2 - 380) ++ "px")
            ]
            [ img [ src "./assets/image/ChatBox.gif", height 40, width 40 ] []
            ]
        , Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewCastleSvg
            , viewRpgCharacter model.character
            , Svg.image
                -- view dark knight
                [ SvgAttr.width "65"
                , SvgAttr.height "65"
                , SvgAttr.x (toString (pixelWidth / 2 - 400))
                , SvgAttr.y (toString (pixelHeight / 2 - 210))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "./assets/image/EvilNPC.png"
                ]
                []
            ]

        -- , viewCharacterPos model.character
        , viewBagCoin model
        , viewTipForDir
        , viewTipForC
        , viewTipForEnter
        ]


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
        ]
        [ div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "100px"
            , HtmlAttr.style "left" "0px"
            ]
            [ img [ src "./assets/image/keyGIF.gif", height 150, width 225 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "400px"
            , HtmlAttr.style "left" "32.5px"
            ]
            [ img [ src "./assets/image/enterGIF.gif", height 80, width 160 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "600px"
            , HtmlAttr.style "left" "72.5px"
            ]
            [ img [ src "./assets/image/cGIF.gif", height 80, width 80 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" (toString (pixelHeight / 2 - 250) ++ "px")
            , HtmlAttr.style "left" (toString (pixelWidth / 2 - 380) ++ "px")
            ]
            [ img [ src "./assets/image/ChatBox.gif", height 40, width 40 ] []
            ]
        , Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewDungeonSvg
            , viewRpgCharacter model.character
            , Svg.image
                -- view dark knight
                [ SvgAttr.width "65"
                , SvgAttr.height "65"
                , SvgAttr.x (toString (pixelWidth / 2 - 400))
                , SvgAttr.y (toString (pixelHeight / 2 - 210))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "./assets/image/EvilNPC.png"
                ]
                []
            ]

        -- , viewCharacterPos model.character
        , viewBagCoin model
        , viewTipForDir
        , viewTipForC
        , viewTipForEnter
        ]


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


viewTipForDir : Html Msg
viewTipForDir =
    div
        [ HtmlAttr.style "top" "280px"
        , HtmlAttr.style "left" "10px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "20px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text "Press ⬆⬅⬇➡ to move" ]


viewTipForC : Html Msg
viewTipForC =
    div
        [ HtmlAttr.style "top" "700px"
        , HtmlAttr.style "left" "5px"
        , HtmlAttr.style "color" "white"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "20px"
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
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "20px"
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



{-
   viewCastle : Model -> Html Msg
   viewCastle model =
       -- Add this when the homepage has been designed
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
           , HtmlAttr.style "left" (String.fromFloat (560 + (w - pixelWidth * r) / 2) ++ "px")
           , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
           , HtmlAttr.style "transform-origin" "0 0"
           , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
           , ("url('./assets/image/Castle.png')"
               ++ " no-repeat fixed "
               ++ " 0px "
               ++ " 0px / "
               ++ (toString startWidth ++ "px " ++ (toString startHeight ++ "px"))
             )
               |> HtmlAttr.style "background"
           ]
           [ Svg.svg
               [ SvgAttr.width "100%"
               , SvgAttr.height "100%"
               ]
               [ viewRpgCharacter model ]
           ]
-}


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
        [ div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" (toString (pixelHeight / 2 - 170) ++ "px")
            , HtmlAttr.style "left" (toString (pixelWidth / 2 - 200) ++ "px")
            ]
            [ img [ src "./assets/image/ChatBox.gif", height 40, width 40 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "100px"
            , HtmlAttr.style "left" "0px"
            ]
            [ img [ src "./assets/image/keyGIF.gif", height 150, width 225 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "400px"
            , HtmlAttr.style "left" "32.5px"
            ]
            [ img [ src "./assets/image/enterGIF.gif", height 80, width 160 ] []
            ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "top" "600px"
            , HtmlAttr.style "left" "72.5px"
            ]
            [ img [ src "./assets/image/cGIF.gif", height 80, width 80 ] []
            ]
        , Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            [ viewShopSvg
            , viewRpgCharacter model.character
            , Svg.image
                -- view shopkeeper
                [ SvgAttr.width "85"
                , SvgAttr.height "85"
                , SvgAttr.x (toString (pixelWidth / 2 - 250))
                , SvgAttr.y (toString (pixelHeight / 2 - 120))
                , SvgAttr.preserveAspectRatio "none"
                , SvgAttr.xlinkHref "./assets/image/HealerRed.png"
                ]
                []
            ]

        -- , viewCharacterPos model.character
        , viewTipForDir
        , viewTipForC
        , viewTipForEnter
        ]



-- viewShopKeeper : Svg Msg
-- viewShopKeeper =
--     Svg.image
--         [ SvgAttr.width "500"
--         , SvgAttr.height "500"
--         , SvgAttr.x (toString (pixelWidth / 2 - 600))
--         , SvgAttr.y (toString (pixelHeight / 2 - 400))
--         , SvgAttr.preserveAspectRatio "none"
--         , SvgAttr.z "2"
--         , SvgAttr.xlinkHref "./assets/image/HealerRed.png"
--         ]
--         []


viewShopSvg : Svg Msg
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



{-
   viewShop : Model -> Html Msg
   viewShop model =
       -- Add this when the homepage has been designed
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
           , HtmlAttr.style "left" (String.fromFloat (560 + (w - pixelWidth * r) / 2) ++ "px")
           , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
           , HtmlAttr.style "transform-origin" "0 0"
           , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
           , ("url('./assets/image/Shop.jpg')"
               ++ " no-repeat fixed "
               ++ " 0px "
               ++ " 0px / "
               ++ (toString startWidth ++ "px " ++ (toString startHeight ++ "px"))
             )
               |> HtmlAttr.style "background"
           ]
           [ Svg.svg
               [ SvgAttr.width "100%"
               , SvgAttr.height "100%"
               ]
               [ viewRpgCharacter model ]
           ]
-}


startWidth : Float
startWidth =
    2000


startHeight : Float
startHeight =
    1000


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
        [ HtmlAttr.style "bottom" "50px"
        , HtmlAttr.style "right" "100px"
        , HtmlAttr.style "color" "orange"
        , HtmlAttr.style "font-family" "Helvetica, Arial, sans-serif"
        , HtmlAttr.style "font-size" "40px"
        , HtmlAttr.style "font-weight" "bold"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "line-height" "60px"
        , HtmlAttr.style "position" "absolute"
        ]
        [ text ("Coins: " ++ toString model.bag.coins) ]
