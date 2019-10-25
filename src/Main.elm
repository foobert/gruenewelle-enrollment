module Main exposing (Model, Msg(..), init, intersectionSelect, main, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Increment
    | Decrement
    | TurnGreen
    | TurnRed


type alias Model =
    { counter : Int
    , other : Int
    , observations : List Observation
    }


type alias Observation =
    { timestamp : String }


init =
    { counter = 0
    , other = 42
    , observations = []
    }


update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 }

        Decrement ->
            { model | counter = model.counter - 1 }

        TurnGreen ->
            { model | observations = List.append model.observations [ Observation "foo" ] }

        TurnRed ->
            model


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
        (List.map observation model.observations)


observation : Observation -> Html msg
observation o =
    li [] [ text o.timestamp ]
