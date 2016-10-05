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


type alias DiceId =
    Int


type alias Model =
    { diceList : List ( DiceId, Dice.Model )
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
    | Remove DiceId
    | SubMsg DiceId Dice.Msg


initDice : ( DiceId, Dice.Model ) -> ( DiceId, Dice.Model )
initDice ( id, _ ) =
    let
        ( newDice, _ ) =
            Dice.init
    in
        ( id, newDice )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Reset ->
            let
                newDiceList =
                    List.map initDice model.diceList
            in
                ( { model | diceList = newDiceList }
                , Cmd.none
                )

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

        Remove diceId ->
            ( { model
                | diceList = List.filter (\( id, _ ) -> diceId /= id) model.diceList
              }
            , Cmd.none
            )

        SubMsg diceId msg ->
            let
                subUpdate (( id, diceModel ) as entry) =
                    if id == diceId then
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


viewDice : ( DiceId, Dice.Model ) -> Html Msg
viewDice ( id, model ) =
    let
        diceWrapperStyle =
            style
                [ ( "width", "5em" )
                , ( "float", "left" )
                , ( "margin", ".5em" )
                , ( "textAlign", "center" )
                ]

        buttonStyle =
            style [ ( "marginTop", ".5em" ) ]
    in
        div
            [ diceWrapperStyle, Html.Attributes.id ("dice-" ++ (toString id)) ]
            [ App.map (SubMsg id) <| Dice.view model
            , button [ onClick (Remove id), buttonStyle ] [ text "Remove" ]
            ]


view : Model -> Html Msg
view model =
    let
        controlsStyle =
            style
                [ ( "textAlign", "left" )
                , ( "margin", "1em" )
                ]

        dices =
            List.map viewDice model.diceList
    in
        div []
            [ div [ controlsStyle ]
                [ button [ onClick Reset ] [ text "Reset All" ]
                , button [ onClick Add ] [ text "Add" ]
                ]
            , div [] dices
            ]
