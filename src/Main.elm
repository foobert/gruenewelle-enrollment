port module Main exposing (Model, Msg(..), init, intersectionSelect, main, update, view)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as E
import Observation
    exposing
        ( Intersection
        , LightPhase(..)
        , Observation
        , encodeObservations
        , lightPhaseToString
        , observationsDecoder
        )
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
    | Intersection Intersection


type alias Model =
    { observations : List Observation
    , currentIntersection : Intersection
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init observations =
    ( Model (initObservations observations) "Kurt Eisener"
    , Cmd.none
    )


initObservations json =
    let
        result =
            log "decoded" (Json.Decode.decodeValue observationsDecoder json)
    in
    Result.withDefault [] result


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TurnGreen ->
            ( model, Task.perform (\t -> NewTime t Green model.currentIntersection) Time.now )

        TurnRed ->
            ( model, Task.perform (\t -> NewTime t Red model.currentIntersection) Time.now )

        NewTime t p i ->
            let
                updatedModel =
                    { model
                        | observations =
                            List.append model.observations
                                [ Observation t p i ]
                    }
            in
            ( log "model" updatedModel
            , persistModel (encodeModel updatedModel)
            )

        Intersection s ->
            ( { model | currentIntersection = s }, Cmd.none )


port persistModel : E.Value -> Cmd msg


encodeModel : Model -> E.Value
encodeModel model =
    E.object [ ( "observations", encodeObservations model.observations ) ]


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


phase : LightPhase -> Html msg
phase l =
    text (lightPhaseToString l)
