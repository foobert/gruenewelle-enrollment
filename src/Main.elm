port module Main exposing (Model, Msg(..), init, intersectionSelect, main, update, view)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Http
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
    | Upload
    | UploadDone (Result Http.Error ())


type alias Model =
    { observations : List Observation
    , currentIntersection : Intersection
    , uploading : Bool
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init observations =
    ( Model (initObservations observations) "Kurt Eisener" False
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

        Upload ->
            ( { model | uploading = True }
            , Http.request
                { method = "PUT"
                , headers = []
                , url = "https://api.jsonbin.io/b/5dea2043cb4ac60420752ec1"
                , body = Http.jsonBody (encodeModel model)
                , expect = Http.expectWhatever UploadDone
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        UploadDone _ ->
            ( { model | uploading = False }, Cmd.none )


port persistModel : E.Value -> Cmd msg


encodeModel : Model -> E.Value
encodeModel model =
    E.object [ ( "observations", encodeObservations model.observations ) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Grüne Welle" ]
            , intersectionSelect model

            --, text ("Current intersection: " ++ model.currentIntersection)
            , div [ class "field", class "is-grouped" ]
                [ div [ class "control" ] [ button [ class "button", onClick TurnRed ] [ text "Red" ] ]
                , div [ class "control" ] [ button [ class "button", onClick TurnGreen ] [ text "Green" ] ]
                ]
            , observationLog model
            , uploadButton model
            ]
        ]


intersectionSelect model =
    div [ class "field" ]
        [ label [ class "label" ] [ text "Intersection" ]
        , div [ class "control" ]
            [ div [ class "select" ]
                [ select [ onInput Intersection ]
                    [ option [] [ text "Kurt Eisener" ]
                    , option [] [ text "Axel" ]
                    , option [] [ text "Löffelfamilie" ]
                    , option [] [ text "Waffeln" ]
                    , option [] [ text "Polizei" ]
                    ]
                ]
            ]
        ]


uploadButton model =
    button (List.append [ class "button", onClick Upload ] (uploadButtonClass model)) [ text "Upload to jsonbin.io" ]


uploadButtonClass model =
    if model.uploading then
        [ class "is-loading" ]

    else
        []


observationLog model =
    div []
        [ p [] [ text ("Collected " ++ String.fromInt (List.length model.observations) ++ " observations so far") ]
        , latestObservation model
        ]


latestObservation model =
    let
        mo =
            List.head (List.reverse model.observations)
    in
    case mo of
        Just o ->
            observation o

        Nothing ->
            div [] []


observation : Observation -> Html msg
observation o =
    p [] [ text (String.concat [ o.intersection, " turned ", lightPhaseToString o.phase, " at ", formatTime o.timestamp ]) ]


formatTime : Posix -> String
formatTime ts =
    let
        h =
            String.padLeft 2 '0' (String.fromInt (Time.toHour utc ts))

        m =
            String.padLeft 2 '0' (String.fromInt (Time.toMinute utc ts))

        s =
            String.padLeft 2 '0' (String.fromInt (Time.toSecond utc ts))
    in
    h ++ ":" ++ m ++ ":" ++ s


phase : LightPhase -> Html msg
phase l =
    text (lightPhaseToString l)
