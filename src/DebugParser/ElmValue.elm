module DebugParser.ElmValue exposing
    ( ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..), toggle, hasNestedValues
    , Path, initPath, indexFromPath, mapValuesWithPath, toggleValueByPath
    )

{-|

@docs ElmValue, ExpandableValue, PlainValue, SequenceType, toggle, hasNestedValues
@docs Path, initPath, indexFromPath, mapValuesWithPath, toggleValueByPath

-}

import List.Extra as List


{-| Elm values that are parsed can be either plain values like Strings and Bools, or they can be expandable values like Records, Dicts etc.

Expandable values has bool as their first parameter, which is used to indicate whether they are expanded or collapsed. This is used in UI
to show either the short version of the expandable value or the full, expanded one. It is part of the parsed type because performance of adding
it later on large models is really costly. This might change in the upcoming versions of this parser.

-}
type ElmValue
    = Plain PlainValue
    | Expandable Bool ExpandableValue


{-| Plain values
-}
type PlainValue
    = ElmString String
    | ElmChar Char
    | ElmNumber Float
    | ElmBool Bool
    | ElmFunction
    | ElmInternals
    | ElmUnit
    | ElmFile String
    | ElmBytes Int


{-| Expandable values
-}
type ExpandableValue
    = ElmSequence SequenceType (List ElmValue)
    | ElmType String (List ElmValue)
    | ElmRecord (List ( String, ElmValue ))
    | ElmDict (List ( ElmValue, ElmValue ))


{-| Sequence type

All List-like structures are the same, so we parsed them into `ElmSequence` type. However we would like to keep the information about which
type the sequence was.

-}
type SequenceType
    = SeqSet
    | SeqList
    | SeqArray
    | SeqTuple


{-| Determines whether value has any nested childrens.
-}
hasNestedValues : ElmValue -> Bool
hasNestedValues value =
    case value of
        Expandable _ expandableValue ->
            case expandableValue of
                ElmSequence _ values ->
                    not <| List.isEmpty values

                ElmRecord _ ->
                    True

                ElmDict values ->
                    not <| List.isEmpty values

                ElmType _ values ->
                    not <| List.isEmpty values

        _ ->
            False


{-| Toggle isExpanded flag for `ExpandableValue`
-}
toggle : ElmValue -> ElmValue
toggle value =
    case value of
        Expandable isExpanded expandableValue ->
            case expandableValue of
                ElmType _ [] ->
                    value

                _ ->
                    Expandable (not isExpanded) expandableValue

        _ ->
            value


{-| Path to uniquely identify nested expandable value. It is used to e.g. toggle specific expandable value, so you can expand and collapse it.
-}
type Path
    = Path (List Int)


{-| Initial path. It is used as the top level starting point
-}
initPath : Path
initPath =
    Path []


{-| Extracts index of nested value from it's path
-}
indexFromPath : Path -> Int
indexFromPath (Path path) =
    List.reverse path
        |> List.head
        |> Maybe.withDefault 0


{-| Useful if you want to render nested values and you would like to make them clickable. Think of this as `List.mapIndex` but with Path instead of index.
-}
mapValuesWithPath : Path -> (Path -> a -> b) -> List a -> List b
mapValuesWithPath (Path parentPath) mapFn values =
    let
        newPath idx =
            Path (parentPath ++ [ idx ])
    in
    List.indexedMap (\idx value -> mapFn (newPath idx) value) values


{-| Given a path and a top level value you can expand and collapse nested expandable values.
-}
toggleValueByPath : Path -> ElmValue -> ElmValue
toggleValueByPath (Path path) value =
    let
        mapNestedValue mapIndex mappedFn =
            case value of
                Expandable isOpened expandableValue ->
                    Expandable isOpened <|
                        case expandableValue of
                            ElmSequence b values ->
                                ElmSequence b <| List.updateIfIndex ((==) mapIndex) mappedFn values

                            ElmRecord values ->
                                ElmRecord <| List.updateIfIndex ((==) mapIndex) (Tuple.mapSecond mappedFn) values

                            ElmDict values ->
                                ElmDict <| List.updateIfIndex ((==) mapIndex) (Tuple.mapSecond mappedFn) values

                            ElmType b typeValues ->
                                case typeValues of
                                    [] ->
                                        ElmType b typeValues

                                    values ->
                                        ElmType b <| List.updateIfIndex ((==) mapIndex) mappedFn values

                _ ->
                    value
    in
    case path of
        [] ->
            toggle value

        [ idx ] ->
            mapNestedValue idx toggle

        idx :: rest ->
            mapNestedValue idx (toggleValueByPath (Path rest))
