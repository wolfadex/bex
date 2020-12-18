module Parser.Extra exposing (char, many, oneOrMore)

import List.Nonempty exposing (Nonempty)
import Parser exposing ((|.), (|=), Parser, Step(..))


oneOrMore : Parser a -> Parser (Nonempty a)
oneOrMore parser =
    Parser.succeed
        (\first rest ->
            List.Nonempty.fromList (first :: rest)
                |> Maybe.withDefault (List.Nonempty.fromElement first)
        )
        |= parser
        |= Parser.loop [] (oneOrMoreHelper parser)


oneOrMoreHelper : Parser a -> List a -> Parser (Step (List a) (List a))
oneOrMoreHelper parser reverseItems =
    Parser.oneOf
        [ Parser.succeed (\item -> Loop (item :: reverseItems))
            |= parser
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse reverseItems))
        ]


many : Parser a -> Parser (List a)
many parser =
    Parser.loop [] (manyHelper parser)


manyHelper : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelper parser reverseItems =
    Parser.oneOf
        [ Parser.succeed (\item -> Loop (item :: reverseItems))
            |= parser
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse reverseItems))
        ]


char : Char -> Parser Char
char c =
    Parser.succeed ()
        |. Parser.chompIf ((==) c)
        |> Parser.getChompedString
        |> Parser.andThen
            (\str ->
                case String.toList str of
                    [ ch ] ->
                        Parser.succeed ch

                    _ ->
                        Parser.problem ("Expected " ++ String.fromChar c ++ " but found " ++ str)
            )
