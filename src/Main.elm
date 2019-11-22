module Main exposing (Model, Msg(..), init, intersectionSelect, main, update, view)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Increment
    | Decrement
    | TurnGreen
    | TurnRed
    | NewTime Posix LightPhase
    | AppendObservation


type LightPhase
    = Green
    | Red


type alias Model =
    { counter : Int
    , other : Int
    , observations : List Observation
    }


type alias Observation =
    { timestamp : Posix
    , phase : LightPhase
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 42 []
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        TurnGreen ->
            ( model, Task.perform (\t -> NewTime t Green) Time.now )

        TurnRed ->
            ( model, Task.perform (\t -> NewTime t Red) Time.now )

        NewTime t p ->
            ( log "model" { model | observations = List.append model.observations [ Observation t p ] }
            , Cmd.none
            )

        AppendObservation ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ intersectionSelect model
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.counter) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick TurnRed ] [ text "RED" ]
        , button [ onClick TurnGreen ] [ text "GREEN" ]
        , observationLog model
        ]


intersectionSelect model =
    div []
        [ text "Intersection"
        , select []
            [ option [] [ text "Kurt Eisener" ]
            , option [] [ text "Axel" ]
            , option [] [ text "Löffelfamilie" ]
            , option [] [ text "Waffeln" ]
            , option [] [ text "Polizei" ]
            ]
        ]


observationLog model =
    div []
        (List.reverse (List.map observation model.observations))


observation : Observation -> Html msg
observation o =
    li [] [ text (String.fromInt (posixToMillis o.timestamp)), phase o.phase ]


phase : LightPhase -> Html msg
phase l =
    case l of
        Red ->
            text "Red"

        Green ->
            text "Green"
