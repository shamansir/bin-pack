module Main exposing (main)

import Browser

import Color
import BinPack as BinPack exposing (..)

import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)




-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Color =
    String



-- type alias Rect =
--    { width : Float
--    , height : Float
--    , color : Color
--    }


type alias Model =
    BinPack Color.Color


init : ( Model, Cmd Msg )
init =
    ( container 300 300
        |> carelessPack ( { width = 10, height = 30 }, Color.black )
        |> carelessPack ( { width = 20, height = 15 }, Color.red )
        |> carelessPack ( { width = 5, height = 25 }, Color.blue )
        |> carelessPack ( { width = 5, height = 25 }, Color.yellow )
        |> carelessPack ( { width = 25, height = 20 }, Color.green )
        |> carelessPack ( { width = 20, height = 10 }, Color.rgb255 130 70 15 )
    , Cmd.none
    )



-- UPDATE


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewItem ( color, bounds ) =
            Svg.rect
                [ S.x <| String.fromFloat bounds.x
                , S.y <| String.fromFloat bounds.y
                , S.width <| String.fromFloat bounds.width
                , S.height <| String.fromFloat bounds.height
                , S.fill <| Color.toCssString color
                , S.strokeWidth "1"
                , S.stroke "black"
                ]
                []
    in
    div
        []
        [ svg [ S.width "300", S.height "300" ] <|
            List.map viewItem <|
                BinPack.toList model
        ]
