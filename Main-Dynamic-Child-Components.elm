module Main exposing (..)

import Dict exposing (..)
import Dice exposing (..)
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
    { diceDict : Dict DiceId Dice.Model
    , uid : Int
    }



-- Init


init : ( Model, Cmd Msg )
init =
    ( Model Dict.empty 0, Cmd.none )



-- Update


type Msg
    = ResetAll
    | Add
    | Remove DiceId
    | DiceMsg DiceId Dice.Msg



-- | RollAll


initDice : a -> b -> Dice.Model
initDice _ _ =
    initialModel


handleDiceMsg : DiceId -> Dice.Msg -> Model -> ( Model, Cmd Msg )
handleDiceMsg diceId msg model =
    let
        diceModel =
            case Dict.get diceId model.diceDict of
                Just val ->
                    val

                Nothing ->
                    initialModel

        ( newDice, fx ) =
            Dice.update msg diceModel

        newDiceDict =
            Dict.insert diceId newDice model.diceDict

        updatedModel =
            ({ model | diceDict = newDiceDict })
    in
        ( updatedModel, Cmd.map (DiceMsg diceId) fx )


handleGlobalMessage : Dice.Msg -> Model -> ( Model, Cmd Msg )
handleGlobalMessage msg model =
    let
        passMsg : DiceId -> Dice.Model -> ( Dice.Model, Cmd Dice.Msg )
        passMsg _ model =
            Dice.update msg model

        updateDiceModel : DiceId -> Dice.Model -> Dice.Model
        updateDiceModel id model =
            passMsg id model |> fst

        getDiceEffect id model =
            passMsg id model |> snd

        effects : Dict DiceId (Cmd Dice.Msg)
        effects =
            Dict.map getDiceEffect model.diceDict

        effectsApplied =
            Dict.map (\id fx -> Cmd.map (DiceMsg id) fx) effects

        updatedModel =
            { model | diceDict = Dict.map updateDiceModel model.diceDict }
    in
        updatedModel ! Dict.values effectsApplied


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ResetAll ->
            handleGlobalMessage Dice.Reset model

        Add ->
            let
                updatedModel =
                    { model
                        | diceDict = Dict.insert model.uid initialModel model.diceDict
                        , uid = model.uid + 1
                    }
            in
                ( updatedModel, Cmd.none )

        Remove diceId ->
            let
                updatedModel =
                    { model | diceDict = Dict.remove diceId model.diceDict }
            in
                ( updatedModel, Cmd.none )

        DiceMsg diceId msg ->
            handleDiceMsg diceId msg model


viewDice : DiceId -> Dice.Model -> Html Msg
viewDice id model =
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
        div [ diceWrapperStyle, Html.Attributes.id ("dice-" ++ (toString id)) ]
            [ App.map (DiceMsg id) <| Dice.view model
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
            Dict.map viewDice model.diceDict |> Dict.values
    in
        div []
            [ div [ controlsStyle ]
                [ button [ onClick ResetAll ] [ text "Reset All" ]
                , button [ onClick Add ] [ text "Add" ]
                ]
            , div [] dices
            ]
