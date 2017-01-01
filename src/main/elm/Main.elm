module Main exposing (..)

import AcyclicDigraph exposing (Node, Edge, Cycle, AcyclicDigraph)
import ArcDiagram
import ArcDiagram.Distance exposing (Distance)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg.Attributes
import Svg exposing (Svg)
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


type alias EdgeFilter =
    { showIncluded : Bool
    , showConflicts : Bool
    , showDuplicates : Bool
    }


type alias Model =
    { graphLookup : GraphLookup
    , selectedNode : Maybe Node
    , edgeFilter : EdgeFilter
    }


init : DependencyGraph -> ( Model, Cmd Msg )
init dependencyGraph =
    ( { graphLookup = toGraphLookup dependencyGraph
      , selectedNode = Nothing
      , edgeFilter =
            { showIncluded = True
            , showConflicts = False
            }
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
    = OmittedForConflict
    | OmittedForDuplicate


type Msg
    = ToggleResolution NodeResolution
    | SelectGraphNode Node


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
                | edgeFilter =
                    updateEdgeFilter nodeResolution model.edgeFilter
              }
            , Cmd.none
            )


updateEdgeFilter : NodeResolution -> EdgeFilter -> EdgeFilter
updateEdgeFilter nodeResolution edgeFilter =
    case nodeResolution of
        OmittedForDuplicate ->
            { edgeFilter | showDuplicates = not edgeFilter.showDuplicates }

        OmittedForConflict ->
            { edgeFilter | showConflicts = not edgeFilter.showConflicts }



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


view : Model -> Html Msg
view model =
    div [ id "main" ]
        [ div [ id "graph" ] [ drawGraph model |> Html.map (\node -> SelectGraphNode node) ]
        , div [ id "filters" ]
            [ header [] [ text "Filter" ]
            , fieldset []
                [ div [ class "title" ] [ text "Resolution" ]
                , label []
                    [ input
                        [ type_ "checkbox"
                        , checked model.edgeFilter.showDuplicates
                        , onClick (ToggleResolution OmittedForDuplicate)
                        ]
                        []
                    , text "Duplicates"
                    ]
                , label []
                    [ input
                        [ type_ "checkbox"
                        , checked model.edgeFilter.showConflicts
                        , onClick (ToggleResolution OmittedForConflict)
                        ]
                        []
                    , text "Conflicts"
                    ]
                ]
            ]
        ]


drawGraph : Model -> Html Node
drawGraph model =
    let
        edgeFilter : Edge -> Dependency -> Bool
        edgeFilter key dependency =
            [ Just "INCLUDED"
            , if model.edgeFilter.showConflicts then
                Just "OMITTED_FOR_CONFLICT"
              else
                Nothing
            , if model.edgeFilter.showDuplicates then
                Just "OMITTED_FOR_DUPLICATE"
              else
                Nothing
            ]
                |> List.filterMap identity
                |> List.member dependency.resolution

        edges =
            model.graphLookup.dependencyDict
                |> Dict.filter edgeFilter
                |> Dict.keys
                |> Set.fromList

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
                ArcDiagram.view
                    layout
                    (paint model graph toLabel)
                    graph


paint : Model -> AcyclicDigraph -> (Node -> String) -> ArcDiagram.Paint
paint model graph toLabel =
    case model.selectedNode of
        Nothing ->
            colors model.graphLookup toLabel

        Just node ->
            (ArcDiagram.Distance.paint
                { viewLabel =
                    \node distance ->
                        viewLabel (toLabel node)
                , colorNode = \node distance -> "#737373"
                , colorEdge = \edge distance -> edgeColor model.graphLookup (Just distance) edge
                }
                graph
                node
            )


edgeColor : GraphLookup -> Maybe Distance -> Edge -> String
edgeColor graphLookup maybeDistance edge =
    case Dict.get edge graphLookup.dependencyDict of
        Just dependency ->
            case maybeDistance of
                Nothing ->
                    case dependency.resolution of
                        "INCLUDED" ->
                            "#010202"

                        "OMITTED_FOR_DUPLICATE" ->
                            "#185AA9"

                        "OMITTED_FOR_CONFLICT" ->
                            "#EE2E2F"

                        _ ->
                            "blue"

                Just aDistance ->
                    case aDistance of
                        Nothing ->
                            case dependency.resolution of
                                "INCLUDED" ->
                                    "#CCCCCC"

                                "OMITTED_FOR_DUPLICATE" ->
                                    "#B8D2EC"

                                "OMITTED_FOR_CONFLICT" ->
                                    "#F2AFAD"

                                _ ->
                                    "blue"

                        Just distance ->
                            if distance >= 0 then
                                case dependency.resolution of
                                    "INCLUDED" ->
                                        "#010202"

                                    "OMITTED_FOR_DUPLICATE" ->
                                        "#185AA9"

                                    "OMITTED_FOR_CONFLICT" ->
                                        "#EE2E2F"

                                    _ ->
                                        "blue"
                            else
                                Debug.crash "nope"

        Nothing ->
            Debug.crash "Could not find dependency in graphLookup.dependencyDict"


colors : GraphLookup -> (Node -> String) -> ArcDiagram.Paint
colors graphLookup toLabel =
    let
        basicPaint =
            ArcDiagram.basicPaint toLabel
    in
        { basicPaint
            | colorEdge = edgeColor graphLookup Nothing
            , colorNode = \node -> "#737373"
            , viewLabel = \node -> viewLabel (toLabel node)
        }


viewLabel : String -> Svg msg
viewLabel string =
    Svg.text_
        [ Svg.Attributes.x "4px"
        , Svg.Attributes.fontFamily "monospace"
        , Svg.Attributes.fontSize "12px"
        , Svg.Attributes.dominantBaseline "middle"
        ]
        [ Svg.text string
        ]


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
