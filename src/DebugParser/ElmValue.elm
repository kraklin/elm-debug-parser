module DebugParser.ElmValue exposing (ElmValue(..), ExpandableValue(..), PlainValue(..), SequenceType(..), toggle, hasNestedValues)

{-|

@docs ElmValue, ExpandableValue, PlainValue, SequenceType, toggle, hasNestedValues

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
