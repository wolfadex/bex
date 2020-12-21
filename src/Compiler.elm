port module Compiler exposing (main)

import Bex.Compiler exposing (Context)
import Bex.Lang exposing (BexModule, BexModulePartial)
import Bex.Parser
import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Platform
import Result.Extra
import Set exposing (Set)
import Time exposing (Posix)


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type Model
    = LoadingConfig String
    | LoadingFiles { projectPath : String, config : Config, filesLoading : Set String, loadedFiles : Dict String BexModulePartial }
    | BodiesParsed { projectPath : String, config : Config, parsed : Dict String BexModule }
    | WritingCompiledCode { projectPath : String, config : Config, parsed : Dict String BexModule, kernelCode : String }
    | Failure


type alias Config =
    { entryFile : String
    , sourceDirectories : List String
    , dependenciesDirect : Dict String Dependency
    , bexVersion : Posix
    }


type alias Dependency =
    { source : String
    , version : Posix
    }


type alias Flags =
    List String



---- INIT ----


init : Flags -> ( Model, Cmd Msg )
init args =
    case args of
        [] ->
            ( Failure, status "Expected a project path" )

        projectPath :: _ ->
            ( LoadingConfig projectPath
            , loadConfig (projectPath ++ "/bex.json")
            )



---- SUBSCRIPTIONS ----


port fileLoaded : (Value -> msg) -> Sub msg


port kernelLoaded : (String -> msg) -> Sub msg


port configLoaded : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fileLoaded FileLoaded
        , kernelLoaded KernelLoaded
        , configLoaded ConfigLoaded
        ]



---- UPDATE ----


port status : String -> Cmd msg


port loadFile : String -> Cmd msg


port writeFile : Value -> Cmd msg


port loadKernel : String -> Cmd msg


port loadConfig : String -> Cmd msg


type Msg
    = FileLoaded Value
    | KernelLoaded String
    | ConfigLoaded Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ConfigLoaded configVal, LoadingConfig projectPath ) ->
            case Json.Decode.decodeValue decodeConfig configVal of
                Ok config ->
                    let
                        entryPath =
                            projectPath ++ "/src/" ++ config.entryFile ++ ".bex"
                    in
                    ( LoadingFiles
                        { config = config
                        , projectPath = projectPath
                        , filesLoading = Set.singleton entryPath
                        , loadedFiles = Dict.empty
                        }
                    , loadFile entryPath
                    )

                Err err ->
                    ( Failure
                    , status ("Failed to parse config: " ++ Json.Decode.errorToString err)
                    )

        ( FileLoaded file, LoadingFiles mod ) ->
            case Json.Decode.decodeValue decodeFile file of
                Ok ( path, content ) ->
                    if Dict.member path mod.loadedFiles then
                        if Set.isEmpty mod.filesLoading then
                            parseBodies mod

                        else
                            Debug.log "carl" ( model, Cmd.none )

                    else
                        case Bex.Parser.parseModuleHeader path content of
                            Ok partiallyParsedModule ->
                                case filePathsFromNames mod.projectPath mod.config partiallyParsedModule.imports of
                                    Ok newFilesToLoad ->
                                        let
                                            newFilesLoading =
                                                List.foldl
                                                    Set.insert
                                                    (Set.remove path mod.filesLoading)
                                                    newFilesToLoad

                                            nextMod =
                                                { mod
                                                    | loadedFiles = Dict.insert path partiallyParsedModule mod.loadedFiles
                                                    , filesLoading = newFilesLoading
                                                }
                                        in
                                        if Set.isEmpty newFilesLoading then
                                            parseBodies nextMod

                                        else
                                            ( LoadingFiles nextMod
                                            , newFilesToLoad
                                                |> List.map loadFile
                                                |> Cmd.batch
                                            )

                                    Err err ->
                                        ( Failure, status err )

                            Err err ->
                                ( Failure, status err )

                Err err ->
                    ( Failure, status (Json.Decode.errorToString err) )

        ( KernelLoaded kernelCode, BodiesParsed { parsed, projectPath, config } ) ->
            let
                compileResult =
                    Bex.Compiler.compile
                        kernelCode
                        config.entryFile
                        (Dict.toList parsed |> List.map Tuple.second)
            in
            case compileResult of
                Ok compiledBex ->
                    ( WritingCompiledCode
                        { projectPath = projectPath
                        , parsed = parsed
                        , kernelCode = kernelCode
                        , config = config
                        }
                    , Cmd.batch
                        [ [ ( "filePath", Json.Encode.string "./bex.js" )
                          , ( "content", Json.Encode.string compiledBex )
                          ]
                            |> Json.Encode.object
                            |> writeFile
                        , status "Writing compiled code to ./bex.js"
                        ]
                    )

                Err err ->
                    ( Failure, status err )

        _ ->
            let
                _ =
                    Debug.log "unhandled msg, model" ( msg, model )
            in
            ( model, Cmd.none )


parseBodies : { a | projectPath : String, config : Config, loadedFiles : Dict String BexModulePartial } -> ( Model, Cmd msg )
parseBodies { projectPath, config, loadedFiles } =
    let
        resultParsedBodies : Dict String (Result String BexModule)
        resultParsedBodies =
            Dict.map Bex.Parser.parseBody loadedFiles

        condenseResultOfParsing =
            Dict.foldl
                (\moduleName parsedModule ->
                    Result.andThen
                        (\res ->
                            Result.map
                                (\val ->
                                    Dict.insert moduleName val res
                                )
                                parsedModule
                        )
                )
                (Ok Dict.empty)
                resultParsedBodies
    in
    case condenseResultOfParsing of
        Ok parsedBodies ->
            ( BodiesParsed
                { projectPath = projectPath
                , parsed = parsedBodies
                , config = config
                }
            , loadKernel
                (projectPath
                    ++ "/dependencies/bex/kernel/"
                    ++ (config.bexVersion
                            |> Time.posixToMillis
                            |> String.fromInt
                       )
                    ++ "/Kernel.js"
                )
            )

        Err err ->
            ( Failure, status err )


filePathsFromNames : String -> Config -> List String -> Result String (List String)
filePathsFromNames path { dependenciesDirect } =
    List.foldl
        (\import_ ->
            Result.andThen
                (\list ->
                    Dict.get import_ dependenciesDirect
                        |> Maybe.map
                            (\{ source, version } ->
                                (path
                                    ++ "/dependencies/"
                                    ++ source
                                    ++ "/"
                                    ++ String.fromInt (Time.posixToMillis version)
                                    ++ "/"
                                    ++ import_
                                    ++ ".bex"
                                )
                                    :: list
                            )
                        |> Result.fromMaybe "Module doesn't exist"
                )
        )
        (Ok [])


decodeConfig : Decoder Config
decodeConfig =
    Json.Decode.map4
        (\entryFile sourceDirectories dependenciesDirect bexVersion ->
            { entryFile = entryFile
            , sourceDirectories = sourceDirectories
            , dependenciesDirect = dependenciesDirect
            , bexVersion = bexVersion
            }
        )
        (Json.Decode.field "entry-file" Json.Decode.string)
        (Json.Decode.field "source-directories" (Json.Decode.list Json.Decode.string))
        (Json.Decode.at [ "dependencies", "direct" ] (Json.Decode.dict decodeDependency))
        (Json.Decode.field "bex-version" decodeVersion)


decodeDependency : Decoder Dependency
decodeDependency =
    Json.Decode.map2 Dependency
        (Json.Decode.field "src" Json.Decode.string)
        (Json.Decode.field "version" decodeVersion)


decodeFile : Decoder ( String, String )
decodeFile =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)


decodeVersion : Decoder Posix
decodeVersion =
    Json.Decode.map Time.millisToPosix
        Json.Decode.int
