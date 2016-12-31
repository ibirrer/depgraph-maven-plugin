module Main exposing (..)

import AcyclicDigraph exposing (Node, Edge, Cycle, AcyclicDigraph)
import ArcDiagram
import ArcDiagram.Distance
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)


main : Program DependencyGraph Model Node
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


init : DependencyGraph -> ( Model, Cmd Node )
init dependencyGraph =
    ( { graphLookup = toGraphLookup dependencyGraph
      , selectedNode = Nothing
      , edgeFilter =
            { showIncluded = True
            , showConflicts = False
            , showDuplicates = False
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


update : Node -> Model -> ( Model, Cmd Node )
update node model =
    ( { model | selectedNode = model.selectedNode |> toggleMaybe node }, Cmd.none )


defaultLayout : ArcDiagram.Layout
defaultLayout =
    ArcDiagram.defaultLayout


layout : ArcDiagram.Layout
layout =
    { defaultLayout
        | labelWidth = 400
    }


view : Model -> Html Node
view model =
    let
        toLabel : Node -> String
        toLabel node =
            Dict.get node model.graphLookup.artifactDict
                |> Maybe.map (\artifact -> (artifact.artifactId ++ ":" ++ artifact.version))
                |> Maybe.withDefault "not found"

        edgeFilter : Edge -> Dependency -> Bool
        edgeFilter key dependency =
            [ if model.edgeFilter.showIncluded then
                Just "INCLUDED"
              else
                Nothing
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

        graphView =
            case AcyclicDigraph.fromEdges edges of
                Err cycles ->
                    viewCycles toLabel cycles

                Ok graph ->
                    let
                        paint =
                            model.selectedNode
                                |> Maybe.map
                                    (ArcDiagram.Distance.basicPaint toLabel graph)
                                |> Maybe.withDefault
                                    (ArcDiagram.basicPaint toLabel)
                    in
                        ArcDiagram.view
                            layout
                            paint
                            graph
    in
        Html.div
            [ Html.Attributes.style
                [ ( "margin", "40px" )
                , ( "font-family", "Helvetica, Arial, san-serif" )
                ]
            ]
            [ graphView
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
