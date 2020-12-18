module Bex exposing (BExpr, Context, eval, init, parse, toString)

import Dict exposing (Dict)
import Html.Attributes exposing (reversed)
import Parser as P exposing ((|.), (|=), Parser, Step(..), Trailing(..))
import Set


type BExpr
    = BInt Int
    | BFunc (Int -> Int)
    | BOper (Int -> Int -> Int)


type Env
    = Env (Dict String BexProgram)


type Context
    = Context InternalContext


type alias InternalContext =
    { stack : Stack, env : Env }


type alias Stack =
    List BExpr


type alias BexProgram =
    InternalContext -> InternalContext


init : Context
init =
    Context { stack = [], env = builtins }



---- EVAL ----


eval : Context -> BexProgram -> Context
eval (Context ctx) prog =
    ctx
        |> prog
        |> Context



---- PRETTY ----


toString : Context -> String
toString (Context { stack, env }) =
    let
        (Env e) =
            env

        stackStr =
            List.map
                (\expr ->
                    case expr of
                        BInt i ->
                            String.fromInt i

                        BFunc _ ->
                            "<function>"

                        BOper _ ->
                            "<function>"
                )
                stack
                |> String.join " "

        envStr =
            e
                |> Dict.keys
                |> Debug.toString
    in
    stackStr ++ "\nEnv:\n" ++ envStr



---- PARSING ----


parse : String -> Result String BexProgram
parse =
    P.run parsePrograms
        -- compose all programs
        >> Result.map (List.foldl (<<) identity)
        >> Result.mapError Debug.toString


parsePrograms : Parser (List BexProgram)
parsePrograms =
    P.succeed identity
        |= P.loop [] parseProgramHelper
        |. P.end


parseProgramHelper : List BexProgram -> Parser (Step (List BexProgram) (List BexProgram))
parseProgramHelper reversePrograms =
    P.oneOf
        [ P.succeed (\e -> Loop (e :: reversePrograms))
            |. P.spaces
            |= parseProgram
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> Done (List.reverse reversePrograms))
        ]


parseProgram : Parser BexProgram
parseProgram =
    P.oneOf
        [ parseBInt
        , parseDefine
        , parseBFunc
        ]


parseBInt : Parser BexProgram
parseBInt =
    P.succeed (\i -> \({ stack } as ctx) -> { ctx | stack = BInt i :: stack })
        -- |= P.int
        |= (P.succeed ()
                |. P.chompWhile Char.isDigit
                |> P.getChompedString
                |> P.andThen
                    (\str ->
                        case String.toInt str of
                            Just i ->
                                P.succeed i

                            Nothing ->
                                P.problem "Expected Int"
                    )
           )


parseBFunc : Parser BexProgram
parseBFunc =
    P.succeed
        (\name ->
            \({ stack, env } as ctx) ->
                let
                    (Env e) =
                        env

                    fn =
                        Dict.get name e
                            |> Maybe.withDefault identity
                in
                fn ctx
        )
        |= (P.succeed ()
                |. P.chompIf (\c -> Char.isAlpha c || List.member c operators && c /= '.')
                |. P.chompWhile (\c -> c /= ' ' && c /= '.')
                |> P.getChompedString
           )


{-|

    def square dup *
    def inc 1 +
    def dec 1 swap -

-}
parseDefine : Parser BexProgram
parseDefine =
    P.succeed
        (\name body ->
            \({ env } as ctx) ->
                let
                    (Env e) =
                        env
                in
                { ctx
                    | env =
                        Env <|
                            case Dict.get name e of
                                Just _ ->
                                    e

                                Nothing ->
                                    Dict.insert
                                        name
                                        (List.foldl (<<) identity body)
                                        e
                }
        )
        |. P.keyword "def"
        |. P.spaces
        |= (P.succeed ()
                |. P.chompIf Char.isAlpha
                |. P.chompWhile Char.isAlphaNum
                |> P.getChompedString
           )
        |. P.spaces
        |= P.loop [] parseDefBody
        |. P.oneOf [ P.symbol "\n", P.end ]


parseDefBody : List BexProgram -> Parser (Step (List BexProgram) (List BexProgram))
parseDefBody reverseBody =
    P.oneOf
        [ P.succeed (\expr -> Loop (expr :: reverseBody))
            |. P.spaces
            |= P.oneOf [ parseBInt, parseBFunc ]
        , P.succeed ()
            |> P.map (\_ -> Done (List.reverse reverseBody))
        ]


operators : List Char
operators =
    [ '+'
    , '-'
    , '*'
    , '/'
    ]



---- BUILTIN ENV ----


builtins : Env
builtins =
    Dict.fromList
        [ ( "+", sumExpr )
        , ( "-", differenceExpr )
        , ( "*", productExpr )
        , ( "/", divisionExpr )
        , ( "drop", dropExpr )
        , ( "swap", swapExpr )
        , ( "dup", dupExpr )
        ]
        |> Env


sumExpr : BexProgram
sumExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt a) :: (BInt b) :: rest ->
                    BInt (a + b) :: rest

                (BInt a) :: rest ->
                    BFunc (\b -> a + b) :: rest

                rest ->
                    BOper (+) :: rest
    }


differenceExpr : BexProgram
differenceExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt a) :: (BInt b) :: rest ->
                    BInt (a - b) :: rest

                (BInt a) :: rest ->
                    BFunc (\b -> a - b) :: rest

                rest ->
                    BOper (-) :: rest
    }


productExpr : BexProgram
productExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt a) :: (BInt b) :: rest ->
                    BInt (a * b) :: rest

                (BInt a) :: rest ->
                    BFunc (\b -> a * b) :: rest

                rest ->
                    BOper (*) :: rest
    }


divisionExpr : BexProgram
divisionExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt a) :: (BInt b) :: rest ->
                    BInt (a // b) :: rest

                (BInt a) :: rest ->
                    BFunc (\b -> a // b) :: rest

                rest ->
                    BOper (//) :: rest
    }


dropExpr : BexProgram
dropExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                _ :: rest ->
                    rest

                [] ->
                    []
    }


swapExpr : BexProgram
swapExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                a :: b :: rest ->
                    b :: a :: rest

                _ ->
                    stack
    }


dupExpr : BexProgram
dupExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                a :: rest ->
                    a :: a :: rest

                [] ->
                    stack
    }
