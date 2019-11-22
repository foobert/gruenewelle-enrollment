port module Main exposing (Model, Msg(..), init, intersectionSelect, main, update, view)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode as E
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
    = TurnGreen
    | TurnRed
    | NewTime Posix LightPhase String
    | AppendObservation
    | Intersection Intersection
    | PersistModel


type LightPhase
    = Green
    | Red


type alias Intersection =
    String


type alias Model =
    { observations : List Observation
    , currentIntersection : Intersection
    }


type alias Observation =
    { timestamp : Posix
    , phase : LightPhase
    , intersection : Intersection
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "Kurt Eisener"
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TurnGreen ->
            ( model, Task.perform (\t -> NewTime t Green model.currentIntersection) Time.now )

        TurnRed ->
            ( model, Task.perform (\t -> NewTime t Red model.currentIntersection) Time.now )

        NewTime t p i ->
            ( log "model"
                { model
                    | observations =
                        List.append model.observations
                            [ Observation t p i ]
                }
            , Cmd.none
            )

        AppendObservation ->
            ( model
            , Cmd.none
            )

        Intersection s ->
            ( { model | currentIntersection = s }, Cmd.none )

        PersistModel ->
            ( model, persistModel (encodeModel model) )


port persistModel : E.Value -> Cmd msg


encodeModel : Model -> E.Value
encodeModel model =
    E.object [ ( "observations", E.list encodeObservation model.observations ) ]


encodeObservation : Observation -> E.Value
encodeObservation o =
    E.object
        [ ( "posix", E.int (posixToMillis o.timestamp) )
        , ( "intersection", E.string o.intersection )
        , ( "phase", E.string (phaseToString o.phase) )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ intersectionSelect model
        , text ("Current intersection: " ++ model.currentIntersection)
        , button [ onClick TurnRed ] [ text "RED" ]
        , button [ onClick TurnGreen ] [ text "GREEN" ]
        , observationLog model
        , button [ onClick PersistModel ] [ text "PERSIST" ]
        ]


intersectionSelect model =
    div []
        [ text "Intersection"
        , select [ onInput Intersection ]
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
    li [] [ text (String.fromInt (posixToMillis o.timestamp)), phase o.phase, text o.intersection ]


phaseToString p =
    case p of
        Red ->
            "Red"

        Green ->
            "Green"


phase : LightPhase -> Html msg
phase l =
    text (phaseToString l)
