module DebugParser exposing (ParsedLog, parse, parseWithOptionalTag)

{-|

@docs ParsedLog, parse, parseWithOptionalTag

-}

import DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..))
import Parser as P exposing ((|.), (|=), DeadEnd, Parser, Step(..))


{-| Alias to represent parsed log.

Tag is part of the log message before the first colon.

-}
type alias ParsedLog =
    { tag : String
    , value : ElmValue
    }


type alias Config =
    { bool : Bool -> ElmValue
    , string : String -> ElmValue
    , char : Char -> ElmValue
    , number : Float -> ElmValue
    , function : ElmValue
    , internals : ElmValue
    , unit : ElmValue
    , bytes : Int -> ElmValue
    , file : String -> ElmValue
    }


defaultConfig : Config
defaultConfig =
    { bool = Plain << ElmBool
    , string = Plain << ElmString
    , char = Plain << ElmChar
    , number = Plain << ElmNumber
    , function = Plain ElmFunction
    , internals = Plain ElmInternals
    , unit = Plain ElmUnit
    , bytes = Plain << ElmBytes
    , file = Plain << ElmFile
    }


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    let
        deadEndToString : DeadEnd -> String
        deadEndToString deadEnd =
            let
                position : String
                position =
                    "row:" ++ String.fromInt deadEnd.row ++ " col:" ++ String.fromInt deadEnd.col ++ "\n"
            in
            case deadEnd.problem of
                P.Expecting str ->
                    "Expecting '" ++ str ++ "' at " ++ position

                P.ExpectingInt ->
                    "ExpectingInt at " ++ position

                P.ExpectingHex ->
                    "ExpectingHex at " ++ position

                P.ExpectingOctal ->
                    "ExpectingOctal at " ++ position

                P.ExpectingBinary ->
                    "ExpectingBinary at " ++ position

                P.ExpectingFloat ->
                    "ExpectingFloat at " ++ position

                P.ExpectingNumber ->
                    "ExpectingNumber at " ++ position

                P.ExpectingVariable ->
                    "ExpectingVariable at " ++ position

                P.ExpectingSymbol str ->
                    "ExpectingSymbol '" ++ str ++ "' at " ++ position

                P.ExpectingKeyword str ->
                    "ExpectingKeyword '" ++ str ++ "' at " ++ position

                P.ExpectingEnd ->
                    "ExpectingEnd at " ++ position

                P.UnexpectedChar ->
                    "UnexpectedChar at " ++ position

                P.Problem str ->
                    "ProblemString '" ++ str ++ "' at " ++ position

                P.BadRepeat ->
                    "BadRepeat at " ++ position
    in
    List.foldl (\str acc -> acc ++ "\n" ++ str) "" (List.map deadEndToString deadEnds)


parseVariableName : Parser String
parseVariableName =
    P.getChompedString <|
        P.succeed ()
            |. P.chompIf Char.isLower
            |. P.chompWhile (\c -> Char.isAlphaNum c || c == '_')


parseTypeName : Parser String
parseTypeName =
    P.getChompedString <|
        P.succeed ()
            |. P.chompIf Char.isUpper
            |. P.chompWhile (\c -> Char.isAlphaNum c || c == '_')


parseNumber : Config -> Parser ElmValue
parseNumber config =
    let
        number =
            P.chompWhile (\char -> Char.isDigit char || char == '.' || char == '+' || char == 'e' || char == '-')
                |> P.getChompedString
                |> P.andThen
                    (\str ->
                        case String.toFloat str of
                            Just float ->
                                P.succeed float

                            Nothing ->
                                P.problem "Unable to parse number"
                    )
    in
    P.oneOf
        [ P.succeed (0 / 0)
            |. P.keyword "NaN"
        , P.succeed (1 / 0)
            |. P.keyword "Infinity"
        , P.succeed -(1 / 0)
            |. P.keyword "-Infinity"
        , P.oneOf
            [ P.succeed negate
                |. P.symbol "-"
                |= number
            , number
            ]
        ]
        |> P.map config.number


parseKeywords : Config -> Parser ElmValue
parseKeywords config =
    P.oneOf
        [ P.succeed config.internals
            |. P.keyword "<internals>"
        , P.succeed config.function
            |. P.keyword "<function>"
        ]


parseList : Parser ElmValue
parseList =
    P.sequence
        { start = "["
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , item = P.lazy (\_ -> parseValue)
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmSequence SeqList listVal
            )


parseArray : Parser ElmValue
parseArray =
    P.sequence
        { start = "Array.fromList ["
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , item = P.lazy (\_ -> parseValue)
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmSequence SeqArray listVal
            )


parseSet : Parser ElmValue
parseSet =
    P.sequence
        { start = "Set.fromList ["
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , item = P.lazy (\_ -> parseValue)
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmSequence SeqSet listVal
            )



{- ---- String parser ------ -}


parseString : Config -> Parser ElmValue
parseString config =
    P.succeed identity
        |. P.token "\""
        |= P.loop [] (stringHelp "\"" isUninteresting)
        |> P.andThen
            (Maybe.map (\str -> P.succeed (config.string str))
                >> Maybe.withDefault (P.problem "One string has no closing double quotes")
            )


stringHelp : String -> (Char -> Bool) -> List String -> Parser (Step (List String) (Maybe String))
stringHelp endString charCheckFn revChunks =
    P.oneOf
        [ P.succeed (\chunk -> Loop (chunk :: revChunks))
            |. P.token "\\"
            |= P.oneOf
                [ P.map (\_ -> "\n") (P.token "n")
                , P.map (\_ -> "\t") (P.token "t")
                , P.map (\_ -> "\u{000D}") (P.token "r")
                , P.map (\_ -> "\u{000B}") (P.token "v")
                , P.map (\_ -> "\u{0000}") (P.token "0")
                , P.map (\_ -> "\\") (P.token "\\")
                , P.map (\_ -> "\"") (P.token "\"")
                , P.succeed String.fromChar
                    |. P.token "u{"
                    |= unicode
                    |. P.token "}"
                ]
        , P.oneOf
            [ P.token endString
                |> P.map (\_ -> Done (Just <| String.concat (List.reverse revChunks)))
            , P.end
                |> P.map (\_ -> Done Nothing)
            ]
        , P.chompWhile charCheckFn
            |> P.getChompedString
            |> P.map
                (\chunk ->
                    Loop (chunk :: revChunks)
                )
        ]


parseFile : Config -> Parser ElmValue
parseFile config =
    P.succeed identity
        |. P.token "<"
        |= P.loop [] (stringHelp ">" (\c -> c /= '>'))
        |> P.andThen
            (Maybe.map (\str -> P.succeed (config.file str))
                >> Maybe.withDefault (P.problem "File has no closing bracket")
            )


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'


unicode : Parser Char
unicode =
    P.getChompedString (P.chompWhile Char.isHexDigit)
        |> P.andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        P.problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        P.succeed (Char.fromCode code)

    else
        P.problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)



{--Char parser --}


parseChar : Config -> Parser ElmValue
parseChar config =
    P.oneOf
        [ P.succeed identity
            |. P.token "'\\''"
            |> P.map (\_ -> config.char '\'')
        , P.succeed identity
            |. P.token "'\\t'"
            |> P.map (\_ -> config.char '\t')
        , P.succeed identity
            |. P.token "'\\n'"
            |> P.map (\_ -> config.char '\n')
        , P.succeed identity
            |. P.token "'\\v'"
            |> P.map (\_ -> config.char '\u{000B}')
        , P.succeed identity
            |. P.token "'\\r'"
            |> P.map (\_ -> config.char '\u{000D}')
        , P.succeed identity
            |. P.token "'\\0'"
            |> P.map (\_ -> config.char '\u{0000}')
        , P.succeed identity
            |. P.token "'"
            |= P.getChompedString (P.chompUntil "'")
            |. P.token "'"
            |> P.map
                (String.toList
                    >> List.reverse
                    >> List.head
                    >> Maybe.withDefault 'x'
                    >> config.char
                )
        ]



{--Record parser --}


parseRecord : Parser ElmValue
parseRecord =
    P.sequence
        { start = "{"
        , end = "}"
        , separator = ","
        , spaces = P.spaces
        , item =
            P.lazy
                (\_ ->
                    P.succeed Tuple.pair
                        |= parseVariableName
                        |. P.token " = "
                        |= parseValue
                )
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmRecord listVal
            )


parseBytes : Config -> Parser ElmValue
parseBytes config =
    -- TODO: the backtrackable can be removed with the combination of file parser
    P.backtrackable <|
        P.succeed config.bytes
            |. P.token "<"
            |= P.int
            |. P.token " bytes>"



{--Custom type parser --}


parseCustomTypeWithoutValue : Config -> Parser ElmValue
parseCustomTypeWithoutValue config =
    P.succeed
        (\name ->
            case name of
                "True" ->
                    config.bool True

                "False" ->
                    config.bool False

                "NaN" ->
                    config.number (0 / 0)

                "Infinity" ->
                    config.number (1 / 0)

                _ ->
                    --NOTE: This is actually not expandable at all. Maybe a tip for a refactoring it later
                    Expandable False <| ElmType name []
        )
        |= parseTypeName


parseCustomType : Config -> Parser ElmValue
parseCustomType config =
    parseTypeName
        |> P.andThen
            (\name ->
                case name of
                    "True" ->
                        P.succeed (config.bool True)

                    "False" ->
                        P.succeed (config.bool False)

                    "NaN" ->
                        P.succeed (config.number (0 / 0))

                    "Infinity" ->
                        P.succeed (config.number (1 / 0))

                    _ ->
                        P.succeed
                            (\list ->
                                Expandable False <| ElmType name (List.reverse list)
                            )
                            |= P.loop [] (typeHelp config)
            )


typeHelp : Config -> List ElmValue -> Parser (Step (List ElmValue) (List ElmValue))
typeHelp config values =
    P.oneOf
        [ P.backtrackable <|
            P.succeed (\value -> Loop (value :: values))
                |. P.token " "
                |= parseValueWithoutCustomType config
        , P.succeed (Done values)
        ]


parseValueWithParenthesis : Config -> Parser ElmValue
parseValueWithParenthesis config =
    P.succeed identity
        |. P.token "("
        |= P.oneOf
            [ P.succeed identity
                |. P.spaces
                |= P.lazy (\_ -> parseValue)
                |> P.andThen
                    (\fstValue ->
                        P.oneOf
                            [ P.succeed identity
                                |. P.spaces
                                |. P.token ","
                                |. P.spaces
                                |= P.lazy (\_ -> parseValue)
                                |> P.andThen
                                    (\sndValue ->
                                        P.succeed identity
                                            |= P.oneOf
                                                [ -- ("x", "y", "z")
                                                  P.succeed identity
                                                    |. P.spaces
                                                    |. P.token ","
                                                    |. P.spaces
                                                    |= P.lazy (\_ -> parseValue)
                                                    |> P.map
                                                        (\rdValue ->
                                                            Expandable False <| ElmSequence SeqTuple [ fstValue, sndValue, rdValue ]
                                                        )
                                                , -- ("x", "y")
                                                  P.succeed
                                                    (Expandable False <| ElmSequence SeqTuple [ fstValue, sndValue ])
                                                ]
                                    )
                            , P.succeed fstValue
                            ]
                    )
            , P.succeed <| config.unit
            ]
        |. P.token ")"



{--Dict parser-}


parseDict : Parser ElmValue
parseDict =
    P.sequence
        { start = "Dict.fromList ["
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , item =
            P.lazy
                (\_ ->
                    P.succeed Tuple.pair
                        |. P.token "("
                        |. P.spaces
                        |= P.lazy (\_ -> parseValue)
                        |. P.spaces
                        |. P.token ","
                        |. P.spaces
                        |= parseValue
                        |. P.spaces
                        |. P.token ")"
                )
        , trailing = P.Forbidden
        }
        |> P.map
            (\listVal ->
                Expandable False <| ElmDict listVal
            )



{- Main value parser -}


parseValueWithoutCustomType : Config -> Parser ElmValue
parseValueWithoutCustomType config =
    P.oneOf
        [ parseRecord
        , parseArray
        , parseSet
        , parseDict
        , parseList
        , parseKeywords config
        , parseCustomTypeWithoutValue config
        , parseNumber config
        , parseValueWithParenthesis config
        , parseChar config
        , parseString config
        , parseBytes config
        , parseFile config
        ]


parseValue : Parser ElmValue
parseValue =
    parseValueWith defaultConfig


parseValueWith : Config -> Parser ElmValue
parseValueWith config =
    P.oneOf
        [ parseRecord
        , parseArray
        , parseSet
        , parseDict
        , parseList
        , parseKeywords config
        , P.lazy (\_ -> parseCustomType config)
        , parseCustomTypeWithoutValue config
        , parseNumber config
        , parseValueWithParenthesis config
        , parseChar config
        , parseString config
        , parseBytes config
        , parseFile config
        ]


{-| Try to parse Debug.log message.
-}
parse : String -> Result String ParsedLog
parse stringToParse =
    stringToParse
        |> String.trim
        |> P.run
            (P.succeed ParsedLog
                |= (P.getChompedString <| P.chompUntil ": ")
                |. P.token ": "
                |= parseValue
                |. P.end
            )
        |> Result.mapError deadEndsToString


{-| Try to parse Debug.log message including the tag. If it is not parsable, try to parse it without the tag.

This one is safer, but it might be slow due to the fact that it is trying two approaches. It should be used with caution.

-}
parseWithOptionalTag : String -> Result String ParsedLog
parseWithOptionalTag stringToParse =
    stringToParse
        |> String.trim
        |> P.run
            (P.oneOf
                [ P.backtrackable
                    (P.succeed ParsedLog
                        |= (P.getChompedString <| P.chompUntil ": ")
                        |. P.token ": "
                        |= parseValue
                        |. P.end
                    )
                , P.succeed (ParsedLog "Debug message")
                    |= parseValue
                    |. P.end
                ]
            )
        |> Result.mapError deadEndsToString
