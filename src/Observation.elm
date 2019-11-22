module Observation exposing
    ( Intersection
    , LightPhase(..)
    , Observation
    , encodeObservations
    , lightPhaseToString
    , observationsDecoder
    )

import Json.Decode as D
import Json.Encode as E
import Time


type alias Observation =
    { timestamp : Time.Posix
    , phase : LightPhase
    , intersection : Intersection
    }


type LightPhase
    = Green
    | Red


type alias Intersection =
    String


observationsDecoder : D.Decoder (List Observation)
observationsDecoder =
    D.field "observations" (D.list observationDecoder)


observationDecoder =
    D.map3 Observation
        posixDecoder
        lightPhaseDecoder
        intersectionDecoder


posixDecoder =
    D.field "posix" (D.map Time.millisToPosix D.int)


lightPhaseDecoder =
    D.field "phase" (D.map stringToLightPhase D.string)


intersectionDecoder =
    D.field "intersection" D.string


lightPhaseToString p =
    case p of
        Red ->
            "Red"

        Green ->
            "Green"


stringToLightPhase s =
    case s of
        "Red" ->
            Red

        "Green" ->
            Green

        _ ->
            Green


encodeObservations : List Observation -> E.Value
encodeObservations os =
    E.list encodeObservation os


encodeObservation o =
    E.object
        [ ( "posix", E.int (Time.posixToMillis o.timestamp) )
        , ( "intersection", E.string o.intersection )
        , ( "phase", E.string (lightPhaseToString o.phase) )
        ]
