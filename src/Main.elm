port module Main exposing (main)

import Bex exposing (BExpr, Context)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Platform
import Result.Extra


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type alias Model =
    { context : Context }



---- INIT ----


init : () -> ( Model, Cmd Msg )
init _ =
    ( { context = Bex.init }
    , sendToTS "PROMPT" (Json.Encode.string "> ")
    )



---- SUBSCRIPTIONS ----


port fromTS : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    fromTS FromTS



---- UPDATE ----


port toTS : Value -> Cmd msg


sendToTS : String -> Value -> Cmd msg
sendToTS action payload =
    [ ( "action", Json.Encode.string action )
    , ( "payload", payload )
    ]
        |> Json.Encode.object
        |> toTS


type Msg
    = FromTS Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromTS val ->
            case Json.Decode.decodeValue decodeTSReponse val of
                Ok ( action, payload ) ->
                    let
                        result =
                            payload
                                |> Bex.parse
                                |> Result.map (Bex.eval model.context)
                    in
                    ( { model
                        | context =
                            result
                                |> Result.mapError (\_ -> model.context)
                                |> Result.Extra.merge
                      }
                    , result
                        |> Result.map Bex.toString
                        |> Result.Extra.merge
                        |> (\s -> s ++ "\n> ")
                        |> Json.Encode.string
                        |> sendToTS "PROMPT"
                    )

                Err err ->
                    ( model
                    , Cmd.batch
                        [ err
                            |> Json.Decode.errorToString
                            |> (\e -> e ++ "> ")
                            |> Json.Encode.string
                            |> sendToTS "PRINT"
                        , sendToTS "PROMPT" (Json.Encode.string "> ")
                        ]
                    )


decodeTSReponse : Decoder ( String, String )
decodeTSReponse =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "action" Json.Decode.string)
        (Json.Decode.field "payload" Json.Decode.string)
