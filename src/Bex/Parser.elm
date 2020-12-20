module Bex.Parser exposing (parse)

import Bex.Lang exposing (BExpr(..), BexModule, Definition)
import List.Nonempty exposing (Nonempty)
import Parser.Advanced as P exposing ((|.), (|=), Parser, Step(..), Token(..), Trailing(..))


type alias BexParser a =
    Parser Context Problem a


type Context
    = File String
    | Module String
    | ModuleBody
    | DefinitionBody


type Problem
    = ExpectingQuote
    | ExpectingOperator
    | ExpectingModuleNameStart
    | ExpectingModuleExposing
    | ExpectingDefinitionNameStart
      -- = Expecting String
    | ExpectingInt
      -- | ExpectingHex
      -- | ExpectingOctal
      -- | ExpectingBinary
      -- | ExpectingFloat
      -- | ExpectingNumber
      -- | ExpectingVariable
      -- | ExpectingSymbol String
      -- | ExpectingKeyword String
    | ExpectingModuleEnd
      -- | UnexpectedChar
    | ExpectedChar Char
      -- | BadRepeat
    | Problem String


parse : String -> String -> Result String BexModule
parse path =
    P.run (P.inContext (File path) parseModule)
        >> Result.mapError Debug.toString


parseModule : BexParser BexModule
parseModule =
    parseModuleName
        |> P.andThen
            (\moduleName ->
                P.succeed (BexModule moduleName)
                    |. oneOrMore (char ' ')
                    |. P.keyword (Token "exposing" ExpectingModuleExposing)
                    |. char '\n'
                    |= P.inContext (Module moduleName) (oneOrMore parseExposeName)
                    |= P.inContext (Module moduleName) (oneOrMore parseModuleBody)
            )


parseModuleName : BexParser String
parseModuleName =
    P.succeed ()
        |. P.chompIf (\c -> Char.isAlpha c && Char.isUpper c) ExpectingModuleNameStart
        |. P.chompWhile Char.isAlphaNum
        |> P.getChompedString


parseExposeName : BexParser String
parseExposeName =
    P.succeed identity
        |. char '\t'
        |= parseDefinitionName
        |. char '\n'


parseModuleBody : BexParser Definition
parseModuleBody =
    P.succeed
        (\name body ->
            { name = name
            , body = List.Nonempty.foldl1 List.Nonempty.append body
            }
        )
        |. oneOrMore (char '\n')
        |= P.inContext ModuleBody parseDefinitionName
        |. char '\n'
        |= P.inContext ModuleBody (oneOrMore parseDefinitionBodyLine)


parseDefinitionName : BexParser String
parseDefinitionName =
    P.succeed ()
        |. P.chompIf (\c -> Char.isAlpha c && Char.isLower c) ExpectingDefinitionNameStart
        |. P.chompWhile Char.isAlphaNum
        |> P.getChompedString


parseDefinitionBodyLine : BexParser (Nonempty BExpr)
parseDefinitionBodyLine =
    P.succeed identity
        |. char '\t'
        |= ([ parseBInt
            , operators
                |> List.map parseOperator
                |> P.oneOf
            , parseWord
            ]
                |> List.map
                    (\par ->
                        P.succeed identity
                            |= P.inContext DefinitionBody (maybeQuotedExpr par)
                            |. many (char ' ')
                    )
                |> P.oneOf
                |> oneOrMore
           )
        |. P.oneOf
            [ char '\n'
                |> P.map
                    (\_ -> ())
            , P.end ExpectingModuleEnd
            ]


maybeQuotedExpr : BexParser BExpr -> BexParser BExpr
maybeQuotedExpr parser =
    P.oneOf
        [ P.backtrackable (parseQuoted parser)
        , parser
        ]


parseQuoted : BexParser BExpr -> BexParser BExpr
parseQuoted parser =
    P.succeed BQuote
        |. P.symbol (Token "`" ExpectingQuote)
        |= parser


parseBInt : BexParser BExpr
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
                        P.problem ExpectingInt
            )


parseOperator : String -> BexParser BExpr
parseOperator op =
    P.symbol (Token op ExpectingOperator)
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


parseWord : BexParser BExpr
parseWord =
    P.map BFunc parseDefinitionName



---- HELPERS ----


oneOrMore : BexParser a -> BexParser (Nonempty a)
oneOrMore parser =
    P.succeed
        (\first rest ->
            List.Nonempty.fromList (first :: rest)
                |> Maybe.withDefault (List.Nonempty.fromElement first)
        )
        |= parser
        |= P.loop [] (oneOrMoreHelper parser)


oneOrMoreHelper : BexParser a -> List a -> BexParser (Step (List a) (List a))
oneOrMoreHelper parser reverseItems =
    P.oneOf
        [ P.succeed (\item -> Loop (item :: reverseItems))
            |= parser
        , P.succeed ()
            |> P.map (\_ -> Done (List.reverse reverseItems))
        ]


many : BexParser a -> BexParser (List a)
many parser =
    P.loop [] (manyHelper parser)


manyHelper : BexParser a -> List a -> BexParser (Step (List a) (List a))
manyHelper parser reverseItems =
    P.oneOf
        [ P.succeed (\item -> Loop (item :: reverseItems))
            |= parser
        , P.succeed ()
            |> P.map (\_ -> Done (List.reverse reverseItems))
        ]


char : Char -> BexParser Char
char c =
    P.succeed ()
        |. P.chompIf ((==) c) (ExpectedChar c)
        |> P.getChompedString
        |> P.andThen
            (\str ->
                case String.toList str of
                    [ ch ] ->
                        P.succeed ch

                    _ ->
                        P.problem (ExpectedChar c)
            )
