module Bex exposing (BExpr, Context, eval, init, parse, toString)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser, Step(..))
import Set


type BExpr
    = BInt Int
    | BFunc (Int -> Int)
    | BOper (Int -> Int -> Int)


type alias Env =
    Dict String BexProgram


type Context
    = Context { stack : Stack, env : Env }


type alias Stack =
    List BExpr


type alias BexProgram =
    Stack -> Stack


init : Context
init =
    Context { stack = [], env = builtins }



---- EVAL ----


eval : Context -> BexProgram -> Context
eval (Context ({ stack } as ctx)) prog =
    stack
        |> prog
        |> (\newStack -> Context { ctx | stack = newStack })



---- PRETTY ----


toString : Context -> String
toString (Context { stack }) =
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
        , parseBFunc
        ]


parseBInt : Parser BexProgram
parseBInt =
    P.succeed (\i -> \stack -> BInt i :: stack)
        |= P.int


parseBFunc : Parser BexProgram
parseBFunc =
    P.succeed
        (\name ->
            Dict.get name builtins
                |> Maybe.withDefault identity
        )
        |= (P.succeed ()
                |. P.chompIf (\c -> Char.isAlpha c || List.member c operators)
                |. P.chompWhile ((/=) ' ')
                |> P.getChompedString
           )


operators : List Char
operators =
    [ '+'
    , '-'
    ]



---- BUILTIN ENV ----


builtins : Env
builtins =
    Dict.fromList
        [ ( "+", sumExpr )
        , ( "-", differenceExpr )
        , ( "drop", dropExpr )
        , ( "swap", swapExpr )
        ]


sumExpr : BexProgram
sumExpr stack =
    case stack of
        (BInt a) :: (BInt b) :: rest ->
            BInt (a + b) :: rest

        (BInt a) :: rest ->
            BFunc (\b -> a + b) :: rest

        rest ->
            BOper (+) :: rest


differenceExpr : BexProgram
differenceExpr stack =
    case stack of
        (BInt a) :: (BInt b) :: rest ->
            BInt (a - b) :: rest

        (BInt a) :: rest ->
            BFunc (\b -> a - b) :: rest

        rest ->
            BOper (-) :: rest


dropExpr : BexProgram
dropExpr stack =
    case stack of
        _ :: rest ->
            rest

        [] ->
            []


swapExpr : BexProgram
swapExpr stack =
    case stack of
        a :: b :: rest ->
            b :: a :: rest

        _ ->
            stack
