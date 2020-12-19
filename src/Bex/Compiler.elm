module Bex.Compiler exposing (BExpr, Context, compile, init, parse)

import Dict exposing (Dict)
import Html.Attributes exposing (reversed)
import List.Nonempty exposing (Nonempty)
import Parser as P exposing ((|.), (|=), Parser, Step(..), Trailing(..))
import Parser.Extra as PE


type alias BexModule =
    { name : String
    , exposing_ : Nonempty String
    , definitions : Nonempty Definition
    }


type alias Definition =
    { name : String
    , body : Nonempty BExpr
    }


type BExpr
    = BInt Int
    | BFunc String
    | BOper String


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


init : Context
init =
    Context
        { stack = []
        , env = Dict.empty -- builtIns
        }



---- COMPILE ----


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
                                        , body = compileFuncBody body
                                        }
                            )
                            ""
                            graph
                in
                Ok
                    ("(function(scope) {\n'use strict';"
                        ++ "\n// BEGIN CORE\n"
                        ++ buildInWordsCompiled
                        ++ "\n"
                        ++ literalCompiledFunc
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
  return Bex__""" ++ moduleName ++ """__main([]);
}"""


compileFuncBody : Nonempty BExpr -> String
compileFuncBody =
    List.Nonempty.map
        (\word ->
            case word of
                BInt i ->
                    "Bex__Core__literal_int(" ++ String.fromInt i ++ ")"

                BOper op ->
                    case op of
                        "+" ->
                            "Bex__Core__operator__add"

                        "-" ->
                            "Bex__Core__operator__subtract"

                        "*" ->
                            "Bex__Core__operator__times"

                        "/" ->
                            "Bex__Core__operator__divide"

                        _ ->
                            "(a) => a"

                BFunc wd ->
                    "Bex__" ++ String.replace "." "__" wd
        )
        >> List.Nonempty.toList
        >> String.join ", "
        >> (\toReduce -> "  return [" ++ toReduce ++ "].reduce((acc, f) => f(acc), stack);")


buildInWordsCompiled : String
buildInWordsCompiled =
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
      , word = "operator__add"
      , body = """  const [a, b, ...rest] = stack;
  return [a + b, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "operator__subtract"
      , body = """  const [a, b, ...rest] = stack;
  return [a - b, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "operator__times"
      , body = """  const [a, b, ...rest] = stack;
  return [a * b, ...rest];"""
      }
    , { moduleName = "Core"
      , word = "operator__divide"
      , body = """  const [a, b, ...rest] = stack;
  if (b === 0) {
    return [0, ...rest];
  } else {
    return [Math.floor(a / b), ...rest];
  }"""
      }
    ]
        |> List.map createCompiledFunc
        |> String.join "\n"


createCompiledFunc : { moduleName : String, word : String, body : String } -> String
createCompiledFunc { moduleName, word, body } =
    "function Bex__" ++ moduleName ++ "__" ++ word ++ "(stack) {\n" ++ body ++ "\n}"


literalCompiledFunc : String
literalCompiledFunc =
    """function Bex__Core__literal_int(i) {
  return function(stack) {
    return [i, ...stack];
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
        (\({ body } as def) ->
            { def
                | body =
                    List.Nonempty.map
                        (\word ->
                            case word of
                                BFunc wd ->
                                    BFunc <|
                                        if memberBy (\d -> d.name == wd) definitions then
                                            name ++ "." ++ wd

                                        else
                                            "Core." ++ wd

                                _ ->
                                    word
                        )
                        body
            }
        )
        definitions


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



---- PARSING ----


parse : String -> Result String BexModule
parse =
    P.run parseModule
        >> Result.mapError Debug.toString


parseModule : Parser BexModule
parseModule =
    P.succeed BexModule
        |= parseModuleName
        |. PE.oneOrMore (PE.char ' ')
        |. P.keyword "exposing"
        |. PE.char '\n'
        |= PE.oneOrMore parseExposeName
        |= PE.oneOrMore parseDefinition


parseModuleName : Parser String
parseModuleName =
    P.succeed ()
        |. P.chompIf (\c -> Char.isAlpha c && Char.isUpper c)
        |. P.chompWhile Char.isAlphaNum
        |> P.getChompedString


parseExposeName : Parser String
parseExposeName =
    P.succeed identity
        |. PE.char '\t'
        |= parseDefinitionName
        |. PE.char '\n'


parseDefinition : Parser Definition
parseDefinition =
    P.succeed
        (\name body ->
            { name = name
            , body = List.Nonempty.foldl1 List.Nonempty.append body
            }
        )
        |. PE.oneOrMore (PE.char '\n')
        |= parseDefinitionName
        |. PE.char '\n'
        |= PE.oneOrMore parseDefinitionBodyLine


parseDefinitionName : Parser String
parseDefinitionName =
    P.succeed ()
        |. P.chompIf (\c -> Char.isAlpha c && Char.isLower c)
        |. P.chompWhile Char.isAlphaNum
        |> P.getChompedString


parseDefinitionBodyLine : Parser (Nonempty BExpr)
parseDefinitionBodyLine =
    P.succeed identity
        |. PE.char '\t'
        |= ([ parseBInt
            , operators
                |> List.map parseOperator
                |> P.oneOf
            , parseWord
            ]
                |> List.map
                    (\par ->
                        P.succeed identity
                            |= par
                            |. PE.many (PE.char ' ')
                    )
                |> P.oneOf
                |> PE.oneOrMore
           )
        |. P.oneOf
            [ PE.char '\n'
                |> P.map
                    (\_ -> ())
            , P.end
            ]


parseBInt : Parser BExpr
parseBInt =
    P.succeed ()
        |. P.chompWhile Char.isDigit
        |> P.getChompedString
        |> P.andThen
            (\str ->
                case String.toInt str of
                    Just i ->
                        P.succeed (BInt i)

                    Nothing ->
                        P.problem "Expected Int"
            )


parseOperator : String -> Parser BExpr
parseOperator op =
    P.symbol op
        |> P.map (\() -> BOper op)


operators : List String
operators =
    [ "+"
    , "-"
    , "*"
    , "/"
    ]


builtinWords : List String
builtinWords =
    [ "drop"
    , "swap"
    , "dup"
    ]


parseWord : Parser BExpr
parseWord =
    P.map BFunc parseDefinitionName
