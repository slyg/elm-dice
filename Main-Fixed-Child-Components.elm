module Main exposing (..)

import Dice
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
    { topDice : Dice.Model
    , bottomDice : Dice.Model
    }



-- Init


init : ( Model, Cmd Msg )
init =
    let
        ( top, topFx ) =
            Dice.init

        ( bottom, bottomFx ) =
            Dice.init
    in
        ( { topDice = top
          , bottomDice = bottom
          }
        , Cmd.batch
            [ Cmd.map Top topFx
            , Cmd.map Bottom bottomFx
            ]
        )



-- Update


type Msg
    = Reset
    | Top Dice.Msg
    | Bottom Dice.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Reset ->
            init

        Top topMsg ->
            let
                ( top, topCmd ) =
                    Dice.update topMsg model.topDice
            in
                ( { model
                    | topDice = top
                  }
                , Cmd.map Top topCmd
                )

        Bottom bottomMsg ->
            let
                ( bottom, bottomCmd ) =
                    Dice.update bottomMsg model.bottomDice
            in
                ( { model
                    | bottomDice = bottom
                  }
                , Cmd.map Bottom bottomCmd
                )



-- View


view : Model -> Html Msg
view model =
    let
        controlsStyle =
            style
                [ ( "textAlign", "left" )
                , ( "margin", "1em" )
                ]

        diceWrapperStyle =
            style
                [ ( "width", "5em" )
                , ( "float", "left" )
                , ( "margin", ".5em" )
                , ( "textAlign", "center" )
                ]
    in
        div []
            [ div [ controlsStyle ] [ button [ onClick Reset ] [ text "Reset" ] ]
            , div [ diceWrapperStyle ] [ App.map Top (Dice.view model.topDice) ]
            , div [ diceWrapperStyle ] [ App.map Bottom (Dice.view model.bottomDice) ]
            ]
