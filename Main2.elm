module Main exposing (..)

import Dice exposing (init)
import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (style)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { diceList : List ( Int, Dice.Model )
    , uid : Int
    }



-- Init


init : ( Model, Cmd Msg )
init =
    ( Model [] 0
    , Cmd.none
    )



-- Update


type Msg
    = Reset
    | Add
    | SubMsg Dice.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Reset ->
            init

        Add ->
            let
                ( newDice, _ ) =
                    Dice.init
            in
                ( { model
                    | diceList = model.diceList ++ [ ( model.uid, newDice ) ]
                    , uid = model.uid + 1
                  }
                , Cmd.none
                )

        SubMsg msg ->
            init



-- let
--   ( newDice, fx ) =
--       Dice.update msg
-- in
--   ( { model
--       |
--     }
--   )
-- View


viewDice : ( Int, Dice.Model ) -> Html Msg
viewDice ( id, model ) =
    let
        diceWrapperStyle =
            style
                [ ( "width", "5em" )
                , ( "margin", "1em auto" )
                , ( "textAlign", "center" )
                ]
    in
        div
            [ diceWrapperStyle
            , Html.Attributes.id ("dice-" ++ (toString id))
            ]
            [ App.map SubMsg <| Dice.view model
            ]


view : Model -> Html Msg
view model =
    let
        controlsStyle =
            style
                [ ( "textAlign", "center" )
                , ( "margin", "1em auto" )
                ]

        dices =
            List.map viewDice model.diceList
    in
        div []
            [ div [ controlsStyle ]
                [ button [ onClick Reset ] [ text "Reset" ]
                , button [ onClick Add ] [ text "Add" ]
                ]
            , div [] dices
            ]
