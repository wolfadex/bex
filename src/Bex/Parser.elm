module Bex.Parser exposing (parse)

import Bex.Lang exposing (BExpr(..), BexModule, Definition)
import List.Nonempty exposing (Nonempty)
import Parser as P exposing ((|.), (|=), Parser, Step(..), Trailing(..))
import Parser.Extra as PE


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
