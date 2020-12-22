module Bex.Parser exposing (parseBody, parseModuleHeader)

import Bex.Lang exposing (BExpr(..), BexModule, BexModulePartial, Definition)
import List.Nonempty exposing (Nonempty)
import Parser.Advanced as P exposing ((|.), (|=), Parser, Step(..), Token(..), Trailing(..))


type alias BexParser a =
    Parser Context Problem a


type Context
    = File String
    | Module String
    | ModuleBodyOf String
    | ModuleBody
    | DefinitionBody


type Problem
    = ExpectingQuote
    | ExpectingOperator
    | ExpectingModuleNameStart
    | ExpectingModuleExposing
    | ExpectingDefinitionNameStart
    | ExpectingInt
      -- = Expecting String
      -- | ExpectingHex
      -- | ExpectingOctal
      -- | ExpectingBinary
      -- | ExpectingFloat
      -- | ExpectingNumber
      -- | ExpectingVariable
      -- | ExpectingSymbol String
      -- | ExpectingKeyword String
      -- | UnexpectedChar
      -- | BadRepeat
    | ExpectingModuleEnd
    | ExpectingWord
    | ExpectedStringEndDoubleQuote
    | ExpectedChar Char
    | ExpectingNewLine
    | ExpectingKeywordImport
    | Problem String



---- PARSE MODULE HEADER ----


parseModuleHeader : String -> String -> Result String BexModulePartial
parseModuleHeader path =
    P.run (P.inContext (File path) parseModulePartial)
        >> Result.mapError Debug.toString


parseModulePartial : BexParser BexModulePartial
parseModulePartial =
    parseModuleName
        |> P.andThen
            (\moduleName ->
                P.succeed (BexModulePartial moduleName)
                    |. oneOrMore (char ' ')
                    |. P.keyword (Token "exposing" ExpectingModuleExposing)
                    |. char '\n'
                    |= P.inContext (Module moduleName) (oneOrMore parseExposeName)
                    |. oneOrMore (char '\n')
                    |= parseImports
                    |= captureBody
                    |. P.end ExpectingModuleEnd
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


parseImports : BexParser (List String)
parseImports =
    P.loop [] parseImportsHelper


parseImportsHelper : List String -> BexParser (Step (List String) (List String))
parseImportsHelper reverseImports =
    P.oneOf
        [ P.succeed (\import_ -> Loop (import_ :: reverseImports))
            |= parseImport
            |. char '\n'
        , P.succeed ()
            |> P.map (\_ -> Done (List.reverse reverseImports))
        ]


parseImport : BexParser String
parseImport =
    P.succeed identity
        |. P.keyword (Token "import" ExpectingKeywordImport)
        |. P.spaces
        |= (P.succeed ()
                |. P.chompUntil (Token "\n" ExpectingNewLine)
                |> P.getChompedString
           )


captureBody : BexParser String
captureBody =
    P.succeed ()
        |. P.chompUntilEndOr "__kernel"
        |> P.getChompedString



---- PARSE MODULE BODY ----


parseBody : String -> BexModulePartial -> Result String BexModule
parseBody path ({ definitions } as modulePartial) =
    definitions
        |> P.run (P.inContext (File path) (parseModuleRemaining modulePartial))
        |> Result.mapError Debug.toString


parseModuleRemaining : BexModulePartial -> BexParser BexModule
parseModuleRemaining { name, exposing_, imports } =
    P.succeed (BexModule name exposing_ imports)
        |= P.inContext (ModuleBodyOf name) (oneOrMore parseModuleBody)


parseModuleBody : BexParser Definition
parseModuleBody =
    P.succeed
        (\name body ->
            { name = name
            , body = List.Nonempty.foldl1 List.Nonempty.append body
            }
        )
        |. many (char '\n')
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
            , parseBString
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
            , P.spaces
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


parseBString : BexParser BExpr
parseBString =
    P.succeed BString
        |. char '"'
        |= (P.succeed ()
                |. P.chompUntil (Token "\"" ExpectedStringEndDoubleQuote)
                |> P.getChompedString
           )
        |. char '"'


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
    , "="
    , ">"
    , ">="
    , "<"
    , "<="
    , "mod"
    , "rem"
    ]


parseWord : BexParser BExpr
parseWord =
    -- P.map BFunc parseDefinitionName
    P.succeed ()
        |. P.chompIf Char.isAlpha ExpectingWord
        |. P.chompWhile (\c -> Char.isAlphaNum c || c == '.')
        |> P.getChompedString
        |> P.map BFunc



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
