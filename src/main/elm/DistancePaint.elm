module DistancePaint exposing (..)

import AcyclicDigraph exposing (AcyclicDigraph, Node, Edge)
import ArcDiagram exposing (Paint)
import ArcDiagram.Distance exposing (..)
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes


defaultDistancePaint : DistancePaint
defaultDistancePaint =
    { viewLabel = \n d -> viewLabelDimmed (isNothing d) (toString n)
    , colorNode = always colorFromDistance
    , colorEdge = always colorFromDistance
    }


basicPaint : (Node -> String) -> AcyclicDigraph -> Node -> Paint
basicPaint toLabel =
    paint
        { defaultDistancePaint
            | viewLabel = \n d -> viewLabelDimmed (isNothing d) (toLabel n)
        }


viewLabelDimmed : Bool -> String -> Svg a
viewLabelDimmed isDimmed string =
    Svg.text_
        [ Svg.Attributes.x "4px"
        , Svg.Attributes.fontFamily "Helvetica, Arial"
        , Svg.Attributes.fontSize "12px"
        , Svg.Attributes.dominantBaseline "middle"
        , Svg.Attributes.fill (labelColor isDimmed)
        ]
        [ Svg.text string
        ]


labelColor : Bool -> String
labelColor isDimmed =
    if isDimmed then
        "rgb(200, 200, 200)"
    else
        "black"


distanceForEdge : Dict Node Int -> Edge -> Distance
distanceForEdge distancesFrom ( a, b ) =
    Maybe.map2
        (\da db ->
            if da >= 0 && db >= 0 then
                Just da
            else if da <= 0 && db <= 0 then
                Just db
            else
                Nothing
        )
        (Dict.get a distancesFrom)
        (Dict.get b distancesFrom)
        |> Maybe.withDefault Nothing


colorFromDistance : Distance -> String
colorFromDistance distance =
    case distance of
        Just d ->
            if d == 0 then
                "black"
            else
                let
                    colorFromAlpha =
                        if d > 0 then
                            blueFromAlpha
                        else
                            redFromAlpha
                in
                    colorFromAlpha <| 1 - ((min 3 (toFloat (abs d)) - 1) * 0.3)

        Nothing ->
            "rgba(0, 0, 0, 0.2)"


blueFromAlpha : Float -> String
blueFromAlpha =
    rgba 35 135 206


redFromAlpha : Float -> String
redFromAlpha =
    rgba 224 69 39


rgba : Int -> Int -> Int -> Float -> String
rgba r g b a =
    "rgba("
        ++ (List.map toString [ r, g, b ] ++ [ toString a ] |> String.join ", ")
        ++ ")"



-- Maybe extra


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Just _ ->
            False

        Nothing ->
            True
