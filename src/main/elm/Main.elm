module Main exposing (..)

import AcyclicDigraph exposing (Node, Edge, Cycle, AcyclicDigraph)
import ArcDiagram
import ArcDiagram.Distance exposing (Distance)
import DistancePaint
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)


main : Program DependencyGraph Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }



-- MODEL


type alias Artifact =
    { id : Int
    , artifactId : String
    , version : String
    }


type alias Dependency =
    { from : Int
    , to : Int
    , resolution : String
    , scopes : List String
    }


type alias DependencyGraph =
    { artifacts : List Artifact
    , dependencies : List Dependency
    }


type alias GraphLookup =
    { artifactDict : Dict Int Artifact
    , dependencyDict : Dict Edge Dependency
    }


type alias ResolutionFilter =
    { showIncluded : Bool
    , showConflicts : Bool
    , showDuplicates : Bool
    }


type alias ScopeFilter =
    { showCompile : Bool
    , showProvided : Bool
    , showRuntime : Bool
    , showTest : Bool
    , showSystem : Bool
    , showImport : Bool
    }


type alias Model =
    { graphLookup : GraphLookup
    , selectedNode : Maybe Node
    , resolutionFilter : ResolutionFilter
    , scopeFilter : ScopeFilter
    , includeFilter : String
    , excludeFilter : String
    }


type Scope
    = Compile
    | Provided
    | Runtime
    | Test
    | System
    | Import


type alias Color =
    String


init : DependencyGraph -> ( Model, Cmd Msg )
init dependencyGraph =
    ( { graphLookup = toGraphLookup dependencyGraph
      , selectedNode = Nothing
      , resolutionFilter =
            { showIncluded = True
            , showDuplicates = False
            , showConflicts = False
            }
      , scopeFilter =
            { showCompile = True
            , showProvided = True
            , showRuntime = True
            , showTest = True
            , showSystem = True
            , showImport = True
            }
      , includeFilter = ""
      , excludeFilter = ""
      }
    , Cmd.none
    )


toGraphLookup : DependencyGraph -> GraphLookup
toGraphLookup dependencyGraph =
    let
        artifactDict =
            dependencyGraph.artifacts
                |> List.map (\artifact -> ( artifact.id, artifact ))
                |> Dict.fromList

        dependencyDict =
            dependencyGraph.dependencies
                |> List.map (\dependency -> ( ( dependency.from, dependency.to ), dependency ))
                |> Dict.fromList
    in
        GraphLookup artifactDict dependencyDict



-- UPDATE


type NodeResolution
    = Included
    | OmittedForConflict
    | OmittedForDuplicate


type Msg
    = ToggleResolution NodeResolution
    | SelectGraphNode Node
    | ToggleScope Scope
    | SetIncludeFilter String
    | SetExcludeFilter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectGraphNode node ->
            ( { model
                | selectedNode =
                    model.selectedNode |> toggleMaybe node
              }
            , Cmd.none
            )

        ToggleResolution nodeResolution ->
            ( { model
                | resolutionFilter =
                    updateResolutionFilter nodeResolution model.resolutionFilter
              }
            , Cmd.none
            )

        ToggleScope scope ->
            ( { model
                | scopeFilter =
                    updateScopeFilter scope model.scopeFilter
              }
            , Cmd.none
            )

        SetIncludeFilter filter ->
            ( { model
                | includeFilter = filter
              }
            , Cmd.none
            )

        SetExcludeFilter filter ->
            ( { model
                | excludeFilter = filter
              }
            , Cmd.none
            )


updateResolutionFilter : NodeResolution -> ResolutionFilter -> ResolutionFilter
updateResolutionFilter nodeResolution edgeFilter =
    case nodeResolution of
        Included ->
            { edgeFilter | showIncluded = not edgeFilter.showIncluded }

        OmittedForDuplicate ->
            { edgeFilter | showDuplicates = not edgeFilter.showDuplicates }

        OmittedForConflict ->
            { edgeFilter | showConflicts = not edgeFilter.showConflicts }


updateScopeFilter : Scope -> ScopeFilter -> ScopeFilter
updateScopeFilter scope scopeFilter =
    case scope of
        Compile ->
            { scopeFilter | showCompile = not scopeFilter.showCompile }

        Provided ->
            { scopeFilter | showProvided = not scopeFilter.showProvided }

        Runtime ->
            { scopeFilter | showRuntime = not scopeFilter.showRuntime }

        Test ->
            { scopeFilter | showTest = not scopeFilter.showTest }

        System ->
            { scopeFilter | showSystem = not scopeFilter.showSystem }

        Import ->
            { scopeFilter | showImport = not scopeFilter.showImport }



-- VIEW


layout : ArcDiagram.Layout
layout =
    let
        defaultLayout =
            ArcDiagram.defaultLayout
    in
        { defaultLayout
            | labelWidth = 400
        }


filterBar : Model -> Html Msg
filterBar model =
    div [ class "filter-bar" ]
        [ div [ class "filter-bar__column filter-bar__column--stretch" ]
            [ div [ class "text-filter" ]
                [ div [ class "text-filter__item" ]
                    [ label [] [ text "Includes: " ]
                    , input [ spellcheck False, onInput SetIncludeFilter ] []
                    ]
                , div [ class "text-filter__item" ]
                    [ label [] [ text "Excludes: " ]
                    , input [ spellcheck False, onInput SetExcludeFilter ] []
                    ]
                ]
            ]
        , div [ class "filter-bar__column" ]
            [ div [ class "checkbox-filter" ]
                [ div [ class "checkbox-filter__title" ] [ text "Resolution: " ]
                , div [ class "checkbox-filter__controls" ]
                    [ div [ class "checkbox-group" ]
                        [ div [ class "checkbox-group__sub" ]
                            [ label []
                                [ input
                                    [ type_ "checkbox"
                                    , checked model.resolutionFilter.showIncluded
                                    , onClick (ToggleResolution Included)
                                    ]
                                    []
                                , text "Include"
                                ]
                            , label [ style [ ( "color", edgeColorDuplicate Dark ) ] ]
                                [ input
                                    [ type_ "checkbox"
                                    , checked model.resolutionFilter.showDuplicates
                                    , onClick (ToggleResolution OmittedForDuplicate)
                                    ]
                                    []
                                , text "Duplicate"
                                ]
                            , label [ style [ ( "color", edgeColorConflict Dark ) ] ]
                                [ input
                                    [ type_ "checkbox"
                                    , checked model.resolutionFilter.showConflicts
                                    , onClick (ToggleResolution OmittedForConflict)
                                    ]
                                    []
                                , text "Conflict"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "filter-bar__column" ]
            [ div [ class "checkbox-filter" ]
                [ div [ class "checkbox-filter__title" ] [ text "Scopes: " ]
                , div [ class "checkbox-filter__controls" ]
                    [ div [ class "checkbox-group" ]
                        [ div [ class "checkbox-group__sub" ]
                            [ scopeCheckbox model.scopeFilter.showCompile Compile "Compile"
                            , scopeCheckbox model.scopeFilter.showProvided Provided "Provided"
                            , scopeCheckbox model.scopeFilter.showRuntime Runtime "Runtime"
                            ]
                        , div [ class "checkbox-group__sub" ]
                            [ scopeCheckbox model.scopeFilter.showTest Test "Test"
                            , scopeCheckbox model.scopeFilter.showSystem System "System"
                            , scopeCheckbox model.scopeFilter.showImport Import "Import"
                            ]
                        ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ filterBar model
        , div [ class "graph" ]
            [ drawGraph model |> Html.map (\node -> SelectGraphNode node) ]
        ]


scopeCheckbox : Bool -> Scope -> String -> Html Msg
scopeCheckbox isChecked scope labelText =
    label []
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , onClick (ToggleScope scope)
            ]
            []
        , text labelText
        ]


filterEdges : Model -> Set Edge
filterEdges model =
    let
        resolutionFilter : Edge -> Dependency -> Bool
        resolutionFilter edge dependency =
            [ toMaybe model.resolutionFilter.showIncluded "INCLUDED"
            , toMaybe model.resolutionFilter.showConflicts "OMITTED_FOR_CONFLICT"
            , toMaybe model.resolutionFilter.showDuplicates "OMITTED_FOR_DUPLICATE"
            ]
                |> List.filterMap identity
                |> List.member dependency.resolution

        scopeFilter : Edge -> Dependency -> Bool
        scopeFilter edge dependency =
            [ toMaybe model.scopeFilter.showCompile "compile"
            , toMaybe model.scopeFilter.showProvided "provided"
            , toMaybe model.scopeFilter.showRuntime "runtime"
            , toMaybe model.scopeFilter.showTest "test"
            , toMaybe model.scopeFilter.showSystem "system"
            , toMaybe model.scopeFilter.showImport "import"
            ]
                |> List.filterMap identity
                |> containsAny dependency.scopes

        includedNodes : List Node
        includedNodes =
            model.graphLookup.artifactDict
                |> Dict.filter
                    (\id artifact ->
                        containsAnySubstring (String.split "," model.includeFilter) artifact.artifactId
                    )
                |> Dict.keys

        excludedNodes : List Node
        excludedNodes =
            model.graphLookup.artifactDict
                |> Dict.filter
                    (\id artifact ->
                        model.excludeFilter
                            /= ""
                            && containsAnySubstring (String.split "," model.excludeFilter) artifact.artifactId
                    )
                |> Dict.keys

        includedNodesFilter : Edge -> Dependency -> Bool
        includedNodesFilter edge dependeny =
            List.member (Tuple.first edge) includedNodes
                && List.member (Tuple.second edge) includedNodes

        excludedNodesFilter : Edge -> Dependency -> Bool
        excludedNodesFilter edge dependeny =
            (not (List.member (Tuple.first edge) excludedNodes))
                && (not (List.member (Tuple.second edge) excludedNodes))
    in
        model.graphLookup.dependencyDict
            |> Dict.filter resolutionFilter
            |> Dict.filter scopeFilter
            |> Dict.filter includedNodesFilter
            |> Dict.filter excludedNodesFilter
            |> Dict.keys
            |> Set.fromList


containsAnySubstring : List String -> String -> Bool
containsAnySubstring substrings string =
    case substrings of
        x :: xs ->
            if String.contains x string then
                True
            else
                containsAnySubstring xs string

        [] ->
            False


containsAny : List a -> List a -> Bool
containsAny items list =
    case items of
        x :: xs ->
            if List.member x list then
                True
            else
                containsAny xs list

        [] ->
            False


toMaybe : Bool -> String -> Maybe String
toMaybe isOn name =
    if isOn then
        Just name
    else
        Nothing


drawGraph : Model -> Html Node
drawGraph model =
    let
        edges =
            filterEdges model

        toLabel : Node -> String
        toLabel node =
            Dict.get node model.graphLookup.artifactDict
                |> Maybe.map (\artifact -> (artifact.artifactId ++ ":" ++ artifact.version))
                |> Maybe.withDefault "not found"
    in
        case AcyclicDigraph.fromEdges edges of
            Err cycles ->
                viewCycles toLabel cycles

            Ok graph ->
                let
                    paint =
                        model.selectedNode
                            |> Maybe.map
                                (DistancePaint.basicPaint toLabel (edgeColorFromDistance model.graphLookup.dependencyDict) graph)
                            |> Maybe.withDefault
                                (paintNothingSelected model toLabel)
                in
                    ArcDiagram.view
                        layout
                        paint
                        graph


paintNothingSelected : Model -> (Node -> String) -> ArcDiagram.Paint
paintNothingSelected model toLabel =
    { viewLabel = (\node -> DistancePaint.viewLabelDimmed False (toLabel node))
    , colorNode = always (edgeColorIncluded Dark)
    , colorEdge = edgeColor model.graphLookup.dependencyDict Middle
    }


edgeColorFromDistance : Dict Edge Dependency -> Edge -> Distance -> String
edgeColorFromDistance dependencyDict edge distance =
    case distance of
        Just d ->
            if d == 0 then
                edgeColor dependencyDict Dark edge
            else
                edgeColor dependencyDict Middle edge

        Nothing ->
            edgeColor dependencyDict Light edge


edgeColor : Dict Edge Dependency -> ColorIntensity -> Edge -> String
edgeColor dependencyDict colorIntensity edge =
    case Dict.get edge dependencyDict of
        Nothing ->
            colorError

        Just dependency ->
            case dependency.resolution of
                "INCLUDED" ->
                    edgeColorIncluded colorIntensity

                "OMITTED_FOR_DUPLICATE" ->
                    edgeColorDuplicate colorIntensity

                "OMITTED_FOR_CONFLICT" ->
                    edgeColorConflict colorIntensity

                _ ->
                    colorError


colorError : Color
colorError =
    "red"


type ColorIntensity
    = Dark
    | Middle
    | Light


edgeColorConflict : ColorIntensity -> Color
edgeColorConflict intesity =
    case intesity of
        Dark ->
            "rgba(238, 46, 47, 0.8)"

        Middle ->
            "rgba(238, 46, 47, 0.4)"

        Light ->
            "rgba(238, 46, 47, 0.1)"


edgeColorIncluded : ColorIntensity -> Color
edgeColorIncluded intesity =
    case intesity of
        Dark ->
            "rgba(1, 2, 2, 0.8)"

        Middle ->
            "rgba(1, 2, 2, 0.4)"

        Light ->
            "rgba(1, 2, 2, 0.1)"


edgeColorDuplicate : ColorIntensity -> Color
edgeColorDuplicate intesity =
    case intesity of
        Dark ->
            "rgba(24, 90, 169, 0.8)"

        Middle ->
            "rgba(24, 90, 169, 0.4)"

        Light ->
            "rgba(24, 90, 169, 0.1)"


viewCycles : (Node -> String) -> List Cycle -> Html a
viewCycles toLabel cycles =
    Html.div
        []
        [ Html.text "Graph has the following cycles:"
        , Html.ol
            []
            (cycles |> List.map (viewCycle toLabel))
        ]


viewCycle : (Node -> String) -> Cycle -> Html a
viewCycle toLabel cycle =
    Html.li
        []
        [ Html.text (cycle |> List.map toLabel |> String.join " -> ") ]


toggleMaybe : a -> Maybe a -> Maybe a
toggleMaybe a ma =
    if ma == Just a then
        Nothing
    else
        Just a
