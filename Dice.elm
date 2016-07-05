module Dice exposing (Model, Msg, init, update, view)

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


view : Model -> Html Msg
view model =
    let
        diceStyle =
            style
                [ ( "textAlign", "center" )
                , ( "fontFamily", "Tahoma" )
                , ( "backgroundColor", "#eee" )
                , ( "padding", "1em" )
                ]
    in
        div [ diceStyle ]
            [ p [] [ text (toString model.dieFace) ]
            , button [ onClick Roll ] [ text "Roll" ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { dieFace = 1
      }
    , Cmd.none
    )
