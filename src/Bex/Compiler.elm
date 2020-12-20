module Bex.Compiler exposing (Context, compile)

import Bex.Lang exposing (BExpr(..), BexModule, Definition)
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


compile : BexModule -> Result String String
compile ({ name, exposing_, definitions } as mod) =
    mod
        |> qualifyNames
        |> buildGraph
        |> Result.andThen
            (\graph ->
                let
                    userDefined =
                        Dict.foldl
                            (\defName body res ->
                                res
                                    ++ "\n"
                                    ++ createCompiledFunc
                                        { moduleName = name
                                        , word = defName
                                        , body = compileFuncBody name body
                                        }
                            )
                            ""
                            graph
                in
                Ok
                    ("(function(scope) {\n'use strict';"
                        ++ "\n// BEGIN CORE\n"
                        ++ builtInWordsCompiled
                        ++ "\n"
                        ++ literalCompiledFunc
                        ++ quoteCompiledFunc
                        ++ runtimeCompiledFunc name
                        ++ "\n// END CORE\n// BEGIN USER"
                        ++ userDefined
                        ++ "\n// END USER\n"
                        ++ exposeRuntime name
                        ++ "}(this));"
                    )
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


compileFuncBody : String -> Nonempty BExpr -> String
compileFuncBody moduleName =
    List.Nonempty.map (compileFunc moduleName)
        >> List.Nonempty.toList
        >> String.join ", "
        >> (\toReduce -> "  return [" ++ toReduce ++ "].reduce((acc, f) => f(acc), stack);")


compileFunc : String -> BExpr -> String
compileFunc moduleName expr =
    case expr of
        BInt i ->
            buildNamespace
                { user = "wolfadex"
                , package = "bex"
                , function = "Core__literal_int"
                }
                ++ "("
                ++ String.fromInt i
                ++ ")"

        BQuote quotedExpr ->
            buildNamespace
                { user = "wolfadex"
                , package = "bex"
                , function = "Core__quote"
                }
                ++ "("
                ++ compileFunc moduleName quotedExpr
                ++ ")"

        BOper op ->
            case op of
                "+" ->
                    buildNamespace
                        { user = "wolfadex"
                        , package = "bex"
                        , function = "Core__operator_add"
                        }

                "-" ->
                    buildNamespace
                        { user = "wolfadex"
                        , package = "bex"
                        , function = "Core__operator_subtract"
                        }

                "*" ->
                    buildNamespace
                        { user = "wolfadex"
                        , package = "bex"
                        , function = "Core__operator_times"
                        }

                "/" ->
                    buildNamespace
                        { user = "wolfadex"
                        , package = "bex"
                        , function = "Core__operator_divide"
                        }

                "=" ->
                    buildNamespace
                        { user = "wolfadex"
                        , package = "bex"
                        , function = "Core__operator_equal"
                        }

                _ ->
                    "(a) => a"

        BFunc wd ->
            buildNamespace
                { user = "wolfadex"
                , package = "bex"
                , function = String.replace "." "__" wd
                }


builtInWordsCompiled : String
builtInWordsCompiled =
    [ { moduleName = "Core"
      , word = "swap"
      , body = """  const [a, b, ...rest] = stack;
  return [b, a, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "drop"
      , body = """  const [a, b, ...rest] = stack;
  return [b, a, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "dup"
      , body = """  const [a, ...rest] = stack;
  return [a, a, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "operator_add"
      , body = """  const [a, b, ...rest] = stack;
  return [a + b, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "operator_subtract"
      , body = """  const [a, b, ...rest] = stack;
  return [a - b, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "operator_times"
      , body = """  const [a, b, ...rest] = stack;
  return [a * b, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "operator_divide"
      , body = """  const [a, b, ...rest] = stack;
  if (b === 0) {
    return [0, ...rest];
  } else {
    return [Math.floor(a / b), ...rest];
  }"""
      }
    , { moduleName = "Core"
      , word = "operator_equal"
      , body = """  const [a, b, ...rest] = stack;
  return [a === b ? 1 : 0, ...rest];""" -- use 0 and 1 because we only work with Ints and Funcs right now
      }
    , { moduleName = "Core"
      , word = "apply"
      , body = """  const [f, ...rest] = stack;
  return f(rest);"""
      }
    , { moduleName = "Core"
      , word = "then"
      , body = """  const [condition, trueCase, falseCase, ...rest] = stack;
  return [condition ? trueCase : falseCase, ...rest]"""
      }
    , { moduleName = "Core"
      , word = "else"
      , body = """  const [condition, trueCase, falseCase, ...rest] = stack;
  return [!condition ? trueCase : falseCase, ...rest]"""
      }
    ]
        |> List.map createCompiledFunc
        |> String.join "\n"


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


literalCompiledFunc : String
literalCompiledFunc =
    "function "
        ++ buildNamespace
            { user = "wolfadex"
            , package = "bex"
            , function = "Core__literal_int"
            }
        ++ """(i) {
  return function(stack) {
    return [i, ...stack];
  }
}"""


quoteCompiledFunc : String
quoteCompiledFunc =
    "function "
        ++ buildNamespace
            { user = "wolfadex"
            , package = "bex"
            , function = "Core__quote"
            }
        ++ """(quotedFn) {
  return function(stack) {
    return [quotedFn, ...stack];
  }
}"""



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
                if memberBy (\d -> d.name == wd) definitions then
                    moduleName ++ "." ++ wd

                else
                    "Core." ++ wd

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
