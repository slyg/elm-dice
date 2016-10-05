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
    | SubMsg Int Dice.Msg


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

        SubMsg msgId msg ->
            let
                subUpdate (( id, diceModel ) as entry) =
                    if id == msgId then
                        let
                            ( newDice, fx ) =
                                Dice.update msg diceModel
                        in
                            ( ( id, newDice )
                            , Cmd.map (SubMsg id) fx
                            )
                    else
                        ( entry, Cmd.none )

                ( newDiceList, fxList ) =
                    model.diceList
                        |> List.map subUpdate
                        |> List.unzip
            in
                { model | diceList = newDiceList } ! fxList


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
            [ App.map (SubMsg id) <| Dice.view model
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