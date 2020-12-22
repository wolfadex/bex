module Bex.Repl exposing (BExpr, Context, eval, init, parse, toString)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser, Step(..), Trailing(..))


type Context
    = Context InternalContext


type alias InternalContext =
    { stack : Stack, env : Env }


type Env
    = Env (Dict String BexProgram)


type alias BexProgram =
    InternalContext -> InternalContext


type alias Stack =
    List BExpr


type BExpr
    = BInt Int
    | BFunc (Int -> Int)
    | BOper (Int -> Int -> Int)
    | BQuote BexProgram


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

                        BQuote _ ->
                            "<quoted function>"
                )
                stack
                |> String.join " "

        envStr =
            e
                |> Dict.keys
                |> Debug.toString
    in
    -- stackStr ++ "\nEnv:\n" ++ envStr
    stackStr



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
        [ parseDefine
        , maybeQuotedExpr parseBInt
        , maybeQuotedExpr parseBFunc
        ]


maybeQuotedExpr : Parser BexProgram -> Parser BexProgram
maybeQuotedExpr parser =
    P.oneOf
        [ P.backtrackable (parseQuoted parser)
        , parser
        ]


parseQuoted : Parser BexProgram -> Parser BexProgram
parseQuoted parser =
    P.succeed (\expr -> \({ stack } as ctx) -> { ctx | stack = BQuote expr :: stack })
        |. P.symbol "`"
        |= parser


parseBInt : Parser BexProgram
parseBInt =
    P.succeed (\i -> \({ stack } as ctx) -> { ctx | stack = BInt i :: stack })
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
        , ( "apply", applyExpr )
        , ( "identity", identityExpr )
        , ( "then", thenExpr )
        , ( "else", elseExpr )
        ]
        |> Env


sumExpr : BexProgram
sumExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt right) :: (BInt left) :: rest ->
                    BInt (left + right) :: rest

                _ ->
                    stack
    }


differenceExpr : BexProgram
differenceExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt right) :: (BInt left) :: rest ->
                    BInt (left - right) :: rest

                _ ->
                    stack
    }


productExpr : BexProgram
productExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt right) :: (BInt left) :: rest ->
                    BInt (left * right) :: rest

                _ ->
                    stack
    }


divisionExpr : BexProgram
divisionExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt right) :: (BInt left) :: rest ->
                    BInt (left // right) :: rest

                _ ->
                    stack
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


quoteExpr : BexProgram
quoteExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                _ ->
                    stack
    }


applyExpr : BexProgram
applyExpr ({ stack } as ctx) =
    let
        ( quotedFn, restStack ) =
            case stack of
                (BQuote fn) :: rest ->
                    ( fn, rest )

                _ ->
                    ( identity, stack )
    in
    quotedFn { ctx | stack = restStack }


thenExpr : BexProgram
thenExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt condition) :: trueCase :: falseCase :: rest ->
                    (if condition /= 0 then
                        trueCase

                     else
                        falseCase
                    )
                        :: rest

                _ ->
                    stack
    }


elseExpr : BexProgram
elseExpr ({ stack } as ctx) =
    { ctx
        | stack =
            case stack of
                (BInt condition) :: trueCase :: falseCase :: rest ->
                    (if condition == 0 then
                        falseCase

                     else
                        trueCase
                    )
                        :: rest

                _ ->
                    stack
    }


identityExpr : BexProgram
identityExpr ctx =
    ctx
