module DebugParser.Path exposing (Path, initPath, indexFromPath, mapValuesWithPath, toggleValueByPath)

{-|

@docs Path, initPath, indexFromPath, mapValuesWithPath, toggleValueByPath

-}

import DebugParser.ElmValue as ElmValue exposing (ElmValue(..), ExpandableValue(..))
import List.Extra as List


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
            ElmValue.toggle value

        [ idx ] ->
            mapNestedValue idx ElmValue.toggle

        idx :: rest ->
            mapNestedValue idx (toggleValueByPath (Path rest))
