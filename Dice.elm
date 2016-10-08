module Dice exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFace : Int
    }



-- UPDATE


type Msg
    = Reset
    | Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        NewFace newFace ->
            ( { model | dieFace = newFace }, Cmd.none )



-- VIEW


dieFace : Int -> String
dieFace x =
    case x of
        1 ->
            "⚀"

        2 ->
            "⚁"

        3 ->
            "⚂"

        4 ->
            "⚃"

        5 ->
            "⚄"

        6 ->
            "⚅"

        _ ->
            ""


view : Model -> Html Msg
view model =
    let
        diceWrapperStyle =
            style
                [ ( "textAlign", "center" )
                , ( "fontFamily", "Tahoma" )
                , ( "backgroundColor", "#eee" )
                , ( "padding", "1em" )
                , ( "borderRadius", ".3em" )
                ]

        diceStyle =
            style
                [ ( "fontSize", "3em" )
                , ( "cursor", "pointer" )
                , ( "margin", "0" )
                ]
    in
        div [ diceWrapperStyle ]
            [ p [ diceStyle, onClick Roll ] [ text (dieFace model.dieFace) ]
            , button [ onClick Roll ] [ text "Roll" ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


initialModel =
    { dieFace = 1 }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )
