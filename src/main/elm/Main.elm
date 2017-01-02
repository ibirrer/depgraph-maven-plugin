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


type alias Color =
    String


init : DependencyGraph -> ( Model, Cmd Msg )
init dependencyGraph =
    ( { graphLookup = toGraphLookup dependencyGraph
      , selectedNode = Nothing
      , edgeFilter =
            { showIncluded = True
            , showDuplicates = False
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
    = Included
    | OmittedForConflict
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
        Included ->
            { edgeFilter | showIncluded = not edgeFilter.showIncluded }

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
                , label [ style [ ( "color", edgeColorConflict Dark ) ] ]
                    [ input
                        [ type_ "checkbox"
                        , checked model.edgeFilter.showConflicts
                        , onClick (ToggleResolution OmittedForConflict)
                        ]
                        []
                    , text "Conflicts"
                    ]
                , label [ style [ ( "color", edgeColorDuplicate Dark ) ] ]
                    [ input
                        [ type_ "checkbox"
                        , checked model.edgeFilter.showDuplicates
                        , onClick (ToggleResolution OmittedForDuplicate)
                        ]
                        []
                    , text "Duplicates"
                    ]
                , label [ style [ ( "color", edgeColorIncluded Dark ) ] ]
                    [ input
                        [ type_ "checkbox"
                        , checked model.edgeFilter.showIncluded
                        , onClick (ToggleResolution Included)
                        ]
                        []
                    , text "Resolved"
                    ]
                ]
            ]
        ]


drawGraph : Model -> Html Node
drawGraph model =
    let
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
    let
        p =
            ArcDiagram.basicPaint toLabel
    in
        { p
            | colorNode = always (edgeColorIncluded Dark)
            , colorEdge = edgeColor model.graphLookup.dependencyDict Middle
        }


edgeColorFromDistance : Dict Edge Dependency -> Edge -> Distance -> String
edgeColorFromDistance dependencyDict edge distance =
    case distance of
        Just _ ->
            edgeColor dependencyDict Dark edge

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
            "rgba(238, 46, 47, 0.2)"


edgeColorIncluded : ColorIntensity -> Color
edgeColorIncluded intesity =
    case intesity of
        Dark ->
            "rgba(1, 2, 2, 0.8)"

        Middle ->
            "rgba(1, 2, 2, 0.4)"

        Light ->
            "rgba(1, 2, 2, 0.2)"


edgeColorDuplicate : ColorIntensity -> Color
edgeColorDuplicate intesity =
    case intesity of
        Dark ->
            "rgba(24, 90, 169, 0.8)"

        Middle ->
            "rgba(24, 90, 169, 0.4)"

        Light ->
            "rgba(24, 90, 169, 0.2)"


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
