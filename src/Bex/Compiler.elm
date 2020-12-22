module Bex.Compiler exposing (Context, builtinWords, compile)

import Bex.Lang as Lang exposing (BExpr(..), BexModule, BexModulePartial, Definition)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (a)
import Html.Attributes exposing (reversed)
import List.Nonempty exposing (Nonempty)
import Parser as P exposing ((|.), (|=), Parser, Step(..), Trailing(..))
import Parser.Extra as PE


type alias Env =
    Dict String BexProgram


type Context
    = Context InternalContext


type alias InternalContext =
    { stack : Stack, env : Env }


type alias Stack =
    List BExpr


type alias BexProgram =
    Stack -> Stack



---- COMPILE ----


buildNamespace : { user : String, package : String, function : String } -> String
buildNamespace { user, package, function } =
    String.join "__" [ user, package, function ]


compile : String -> String -> List BexModule -> Result String String
compile kernelCode entryModule bexModules =
    let
        dependencyGraph =
            buildDependencyGraph bexModules
    in
    bexModules
        |> doCompiling dependencyGraph Dict.empty Dict.empty
        |> Result.map
            (\userDefineds ->
                "(function(scope) {\n'use strict';"
                    ++ "\n// BEGIN CORE\n"
                    ++ kernelCode
                    ++ "\n"
                    ++ runtimeCompiledFunc entryModule
                    ++ "\n// END CORE\n// BEGIN USER"
                    ++ String.join "\n" userDefineds
                    ++ "\n// END USER"
                    ++ exposeRuntime entryModule
                    ++ "}(this));"
            )


doCompiling : Dict String (List String) -> Dict String String -> Dict String (List Definition) -> List BexModule -> Result String (List String)
doCompiling dependencyGraph compiledModules envDefinitions modules =
    let
        ( toCompileNames, remainingGrpah ) =
            Dict.partition (\_ imports -> List.isEmpty imports) dependencyGraph
                |> Tuple.mapFirst Dict.keys

        ( toCompileModules, remainingModules ) =
            List.partition (\{ name } -> List.member name toCompileNames) modules

        waitingGraphs =
            Dict.map
                (\_ imports -> List.filter (\import_ -> not <| List.member import_ toCompileNames) imports)
                remainingGrpah
    in
    case toCompileModules of
        [] ->
            Ok (Dict.values compiledModules)

        _ ->
            toCompileModules
                |> List.foldl
                    (\module_ ->
                        Result.andThen
                            (\( compiledMods, envDefs ) ->
                                Result.map
                                    (\compiledMod ->
                                        ( Dict.insert module_.name compiledMod compiledMods
                                        , Dict.insert module_.name (List.Nonempty.toList module_.definitions) envDefs
                                        )
                                    )
                                    (compileModule envDefs module_)
                            )
                    )
                    (Ok ( compiledModules, envDefinitions ))
                |> Result.andThen
                    (\( compiled, newEnvDef ) ->
                        doCompiling waitingGraphs compiled newEnvDef remainingModules
                    )


buildDependencyGraph : List BexModule -> Dict String (List String)
buildDependencyGraph =
    List.foldl
        (\{ name, imports } ->
            Dict.insert name imports
        )
        Dict.empty


compileModule : Dict String (List Definition) -> BexModule -> Result String String
compileModule envDefinitions ({ name, definitions } as mod) =
    mod
        |> qualifyNames envDefinitions
        |> buildCompiledExprs
        |> Result.map
            (Dict.foldl
                (\defName body res ->
                    res
                        ++ "\n"
                        ++ createCompiledFunc
                            { moduleName = name
                            , word = defName
                            , body = compileFuncBody body
                            }
                )
                ""
                >> (++) ("\n// BEGIN USER MOD " ++ name)
                >> (\code -> code ++ "\n// END USER MOD " ++ name)
            )


exposeRuntime : String -> String
exposeRuntime moduleName =
    """
if (scope['Bex'] == null) {
  scope['Bex'] = {};
}
if (scope['Bex']['""" ++ moduleName ++ """'] == null) {
  scope['Bex']['""" ++ moduleName ++ """'] = {};
}
return scope['Bex']['""" ++ moduleName ++ """']['run'] = Bex_run;
"""


runtimeCompiledFunc : String -> String
runtimeCompiledFunc moduleName =
    """
function Bex_run() {
  return """
        ++ buildNamespace
            { user = "wolfadex"
            , package = "bex"
            , function = moduleName ++ "__main"
            }
        ++ """([]);
}"""


compileFuncBody : Nonempty BExpr -> String
compileFuncBody =
    List.Nonempty.map compileFunc
        >> List.Nonempty.toList
        >> String.join ", "
        >> (\toReduce -> "  const fns = [" ++ toReduce ++ """];
    for (let i = 0; i < fns.length; i++) {
      stack = fns[i](stack);
    }
    return stack;""")


compileFunc : BExpr -> String
compileFunc expr =
    case expr of
        BInt i ->
            "__kernel__literal_int(" ++ String.fromInt i ++ ")"

        BString str ->
            "__kernel__literal_string(\"" ++ str ++ "\")"

        BQuote quotedExpr ->
            "__kernel__quote(" ++ compileFunc quotedExpr ++ ")"

        BOper op ->
            case op of
                "+" ->
                    "__kernel__operator_add"

                "-" ->
                    "__kernel__operator_subtract"

                "*" ->
                    "__kernel__operator_times"

                "/" ->
                    "__kernel__operator_divide"

                "=" ->
                    "__kernel__operator_equal"

                ">" ->
                    "__kernel__operator_greater_then"

                ">=" ->
                    "__kernel__operator_greater_then_or_equal"

                "<" ->
                    "__kernel__operator_less_then"

                "<=" ->
                    "__kernel__operator_less_then_or_equal"

                "mod" ->
                    "__kernel__operator_mod"

                "rem" ->
                    "__kernel__operator_rem"

                _ ->
                    "(a) => a"

        BFunc wd ->
            wd


createCompiledFunc : { moduleName : String, word : String, body : String } -> String
createCompiledFunc { moduleName, word, body } =
    "function "
        ++ buildNamespace
            { user = "wolfadex"
            , package = "bex"
            , function = moduleName ++ "__" ++ word
            }
        ++ "(stack) {\n"
        ++ body
        ++ "\n}"



---- REDUCE ----


buildCompiledExprs : Nonempty Definition -> Result String (Dict String (Nonempty BExpr))
buildCompiledExprs =
    List.Nonempty.foldl
        (\{ name, body } ->
            Result.andThen
                (\env ->
                    case Dict.get name env of
                        Nothing ->
                            Dict.insert name body env |> Ok

                        Just _ ->
                            Err ("definition " ++ name ++ " already exists")
                )
        )
        (Ok Dict.empty)


qualifyNames : Dict String (List Definition) -> BexModule -> Nonempty Definition
qualifyNames envDefinitions { name, definitions } =
    List.Nonempty.map
        (\({ body } as def) -> { def | body = List.Nonempty.map (qualifyName envDefinitions name definitions) body })
        definitions


qualifyName : Dict String (List Definition) -> String -> Nonempty Definition -> BExpr -> BExpr
qualifyName envDefinitions moduleName definitions expr =
    case expr of
        BFunc wd ->
            BFunc <|
                if List.member wd builtinWords then
                    "__kernel__" ++ wd

                else if memberBy (\d -> d.name == wd) definitions then
                    buildNamespace
                        { user = "wolfadex"
                        , package = "bex"
                        , function = String.replace "." "__" (moduleName ++ "." ++ wd)
                        }

                else
                    let
                        maybeWord =
                            String.split "." wd
                                |> List.reverse
                                |> (\parts ->
                                        case parts of
                                            [] ->
                                                []

                                            [ a ] ->
                                                [ a ]

                                            a :: rest ->
                                                [ rest
                                                    |> List.reverse
                                                    |> String.join "."
                                                , a
                                                ]
                                   )
                                |> (\partitionedWord ->
                                        case partitionedWord of
                                            [ modName, word ] ->
                                                Dict.get modName envDefinitions
                                                    |> Maybe.andThen
                                                        (\defs ->
                                                            findBy (.name >> (==) word) defs
                                                                |> Maybe.map (\_ -> String.join "." [ modName, word ])
                                                        )

                                            _ ->
                                                Nothing
                                   )
                    in
                    case maybeWord of
                        Just word ->
                            buildNamespace
                                { user = "wolfadex"
                                , package = "bex"
                                , function = String.replace "." "__" word
                                }

                        Nothing ->
                            "ERROR_" ++ Debug.log "error word" wd

        BQuote quotedExpr ->
            qualifyName envDefinitions moduleName definitions quotedExpr
                |> BQuote

        _ ->
            expr


findBy : (a -> Bool) -> List a -> Maybe a
findBy predicate ls =
    case ls of
        [] ->
            Nothing

        a :: rest ->
            if predicate a then
                Just a

            else
                findBy predicate rest


memberBy : (a -> Bool) -> Nonempty a -> Bool
memberBy fn =
    List.Nonempty.toList >> memberByHelper fn


memberByHelper : (a -> Bool) -> List a -> Bool
memberByHelper fn ls =
    case ls of
        [] ->
            False

        a :: rest ->
            fn a || memberByHelper fn rest


builtinWords : List String
builtinWords =
    [ "drop"
    , "swap"
    , "dup"
    , "rotate"
    , "over"
    , "apply"
    , "then"
    , "emit"
    , "identity"
    ]
