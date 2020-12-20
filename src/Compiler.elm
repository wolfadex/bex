port module Compiler exposing (main)

import Bex.Compiler exposing (Context)
import Bex.Parser
import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Platform
import Result.Extra


main : Program Args Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type alias Model =
    { files : Dict String String }


type alias Args =
    List String



---- INIT ----


init : Args -> ( Model, Cmd Msg )
init args =
    case args of
        [] ->
            ( { files = Dict.empty }, status "Expected an entry file" )

        entryFilePath :: _ ->
            ( { files = Dict.empty }
            , loadFile entryFilePath
            )



---- SUBSCRIPTIONS ----


port fileLoaded : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    fileLoaded FileLoaded



---- UPDATE ----


port status : String -> Cmd msg


port loadFile : String -> Cmd msg


port writeFile : Value -> Cmd msg


type Msg
    = FileLoaded Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileLoaded file ->
            case Json.Decode.decodeValue decodeFile file of
                Ok ( path, content ) ->
                    ( { model | files = Dict.insert path content model.files }
                    , Bex.Parser.parse path content
                        |> Result.andThen Bex.Compiler.compile
                        |> Result.map
                            (\compiledBex ->
                                [ ( "filePath", Json.Encode.string "./bex.js" )
                                , ( "content", Json.Encode.string compiledBex )
                                ]
                                    |> Json.Encode.object
                                    |> writeFile
                            )
                        |> Result.mapError status
                        |> Result.Extra.merge
                    )

                Err err ->
                    Debug.todo "handle file load decode errpr"



-- FromTS val ->
--     case Json.Decode.decodeValue decodeTSReponse val of
--         Ok ( action, payload ) ->
--             let
--                 result =
--                     payload
--                         |> Bex.parse
--                         |> Result.map (Bex.eval model.context)
--             in
--             ( { model
--                 | context =
--                     result
--                         |> Result.mapError (\_ -> model.context)
--                         |> Result.Extra.merge
--               }
--             , result
--                 |> Result.map Bex.toString
--                 |> Result.Extra.merge
--                 |> (\s -> s ++ "\n> ")
--                 |> Json.Encode.string
--                 |> sendToTS "PROMPT"
--             )
--         Err err ->
--             ( model
--             , Cmd.batch
--                 [ err
--                     |> Json.Decode.errorToString
--                     |> (\e -> e ++ "> ")
--                     |> Json.Encode.string
--                     |> sendToTS "PRINT"
--                 , sendToTS "PROMPT" (Json.Encode.string "> ")
--                 ]
--             )


decodeFile : Decoder ( String, String )
decodeFile =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
