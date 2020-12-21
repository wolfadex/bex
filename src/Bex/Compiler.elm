module Bex.Compiler exposing (Context, compile)

import Bex.Lang as Lang exposing (BExpr(..), BexModule, BexModulePartial, Definition)
import Dict exposing (Dict)
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
    bexModules
        |> List.foldl
            (\module_ ->
                Result.andThen
                    (\compiledMods ->
                        Result.map
                            (\compiledMod ->
                                compiledMod :: compiledMods
                            )
                            (compileModule module_)
                    )
            )
            (Ok [])
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


compileModule : BexModule -> Result String String
compileModule ({ name, exposing_, definitions } as mod) =
    mod
        |> qualifyNames
        |> buildGraph
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
            "__kernel__literal_int("
                ++ String.fromInt i
                ++ ")"

        BQuote quotedExpr ->
            "__kernel__quote("
                ++ compileFunc quotedExpr
                ++ ")"

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


buildGraph : Nonempty Definition -> Result String (Dict String (Nonempty BExpr))
buildGraph =
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


qualifyNames : BexModule -> Nonempty Definition
qualifyNames { name, definitions } =
    List.Nonempty.map
        (\({ body } as def) -> { def | body = List.Nonempty.map (qualifyName name definitions) body })
        definitions


qualifyName : String -> Nonempty Definition -> BExpr -> BExpr
qualifyName moduleName definitions expr =
    case expr of
        BFunc wd ->
            BFunc <|
                if List.member wd Lang.builtinWords then
                    "__kernel__" ++ wd

                else if memberBy (\d -> d.name == wd) definitions then
                    buildNamespace
                        { user = "wolfadex"
                        , package = "bex"
                        , function = String.replace "." "__" (moduleName ++ "." ++ wd)
                        }

                else
                    "ERROR_" ++ wd

        BQuote quotedExpr ->
            qualifyName moduleName definitions quotedExpr
                |> BQuote

        _ ->
            expr


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
