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


type alias Model =
    { graph : ( Dict Node String, Set Edge )
    , selectedNode : Maybe Node
    }


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


init : DependencyGraph -> ( Model, Cmd Node )
init dependencyGraph =
    ( Model
        ( labels dependencyGraph.artifacts
        , edges dependencyGraph.dependencies
        )
        Nothing
    , Cmd.none
    )


labels : List Artifact -> Dict Node String
labels artifacts =
    artifacts
        |> List.map (\artifact -> ( artifact.id, artifact.artifactId ++ "@" ++ artifact.version ))
        |> Dict.fromList


edges : List Dependency -> Set Edge
edges dependencies =
    dependencies
        |> List.map (\dependency -> ( dependency.from, dependency.to ))
        |> Set.fromList



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
view { graph, selectedNode } =
    drawGraph graph selectedNode


drawGraph : ( Dict Int String, Set Edge ) -> Maybe Node -> Html Node
drawGraph ( labels, edges ) selectedNode =
    let
        toLabel =
            (flip Dict.get) labels >> Maybe.withDefault ""

        graphView =
            case AcyclicDigraph.fromEdges edges of
                Err cycles ->
                    viewCycles toLabel cycles

                Ok graph ->
                    let
                        paint =
                            selectedNode
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
