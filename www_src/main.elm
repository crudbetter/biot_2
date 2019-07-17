port module Main exposing (Model, Msg(..), init, main, parseEcho, positionNodes, positionedNodeKvpDecoder, positionedNodeToSvgAttrs, px, renderNodes, subscriptions, update, view, wsEcho)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListExtra
import NodeDict exposing (NodeDict, PositionedNode, SizedNode)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Time


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type alias Model =
    { nodeDict : NodeDict
    , edges : List Edge
    , operatorDataDict : Dict String (List OperatorDatum)
    , tickMillis : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nodeDict =
            NodeDict.singleton
                "switchboard"
                { width = 100, height = 100 }
      , edges = []
      , operatorDataDict = Dict.empty
      , tickMillis = 0
      }
    , Cmd.none
    )


type Msg
    = Echo String
    | Foo (Result Decode.Error DagreResult)
    | Tick Time.Posix


positionedNodeKvpDecoder : Decoder (List ( String, PositionedNode ))
positionedNodeKvpDecoder =
    Decode.keyValuePairs NodeDict.positionedNodeDecoder


type alias Point =
    { x : Float, y : Float }


pointDecoder : Decoder Point
pointDecoder =
    Decode.map2 Point
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


type alias Edge =
    List Point


edgeDecoder : Decoder (List Edge)
edgeDecoder =
    Decode.list <| Decode.field "points" <| Decode.list pointDecoder


type alias OperatorDatum =
    ( Int, Int )


type alias DagreResult =
    { edges : List Edge
    , nodes : List ( String, PositionedNode )
    }


dagreResultDecoder : Decoder DagreResult
dagreResultDecoder =
    Decode.map2 DagreResult
        (Decode.field "edges" edgeDecoder)
        (Decode.field "nodes" positionedNodeKvpDecoder)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ wsEcho Echo
        , renderNodes (Decode.decodeValue dagreResultDecoder >> Foo)
        , Time.every 1000 Tick
        ]


port positionNodes : Encode.Value -> Cmd msg


port renderNodes : (Encode.Value -> msg) -> Sub msg


port wsEcho : (String -> msg) -> Sub msg


parseEcho : String -> Model -> Model
parseEcho value model =
    case String.split " " value of
        [ "device_connected", vdeviceId ] ->
            { model
                | nodeDict =
                    NodeDict.insertNode ("operator_" ++ vdeviceId) { height = 50, width = 50 } model.nodeDict
                        |> NodeDict.insertNode ("device_" ++ vdeviceId) { height = 50, width = 50 }
            }

        [ "device_data_recv", vdeviceId, ts, v ] ->
            let
                operatorDatum =
                    ( Maybe.withDefault 0 <| String.toInt ts
                    , Maybe.withDefault 0 <| String.toInt v
                    )
            in
            { model
                | operatorDataDict =
                    Dict.update vdeviceId
                        (\mData ->
                            case mData of
                                Just data ->
                                    Just (data ++ [ operatorDatum ])

                                Nothing ->
                                    Just (List.singleton operatorDatum)
                        )
                        model.operatorDataDict
            }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Echo value ->
            let
                updatedModel =
                    parseEcho value model

                mEncodedNodes =
                    NodeDict.encodeNodesToBePositioned updatedModel.nodeDict
            in
            case mEncodedNodes of
                Nothing ->
                    ( updatedModel, Cmd.none )

                Just encodedNodes ->
                    ( updatedModel, positionNodes encodedNodes )

        Foo result ->
            case result of
                Ok { nodes, edges } ->
                    let
                        updatedModel =
                            { model
                                | nodeDict =
                                    List.foldl
                                        (\( k, pn ) acc ->
                                            NodeDict.positionNode k pn acc
                                        )
                                        model.nodeDict
                                        nodes
                                , edges = edges
                            }
                    in
                    ( updatedModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Tick tickPosix ->
            let
                tickMillis =
                    Time.posixToMillis tickPosix

                time =
                    tickMillis - 5000
            in
            ( { model
                | tickMillis = tickMillis
                , operatorDataDict =
                    Dict.toList model.operatorDataDict
                        |> List.map
                            (\( k, datumList ) ->
                                ( k, ListExtra.dropWhile (\( ts, v ) -> ts < time) datumList )
                            )
                        |> Dict.fromList
              }
            , Cmd.none
            )


px : Float -> String
px value =
    String.fromFloat value ++ "px"


positionedNodeToSvgAttrs : PositionedNode -> List (Svg.Attribute msg)
positionedNodeToSvgAttrs { height, width, x, y } =
    [ SvgA.x <| String.fromFloat <| x - (width / 2)
    , SvgA.y <| String.fromFloat <| y - (height / 2)
    , SvgA.width <| String.fromFloat width
    , SvgA.height <| String.fromFloat height
    , SvgA.rx "5"
    , SvgA.ry "5"
    , SvgA.fillOpacity "0"
    , SvgA.stroke "teal"
    ]


view : Model -> Html msg
view model =
    let
        toRect : ( String, PositionedNode ) -> List (Svg msg)
        toRect =
            \( text, node ) ->
                [ Svg.rect (positionedNodeToSvgAttrs node) []
                , Svg.text_
                    [ SvgA.x <| String.fromFloat node.x
                    , SvgA.y <| String.fromFloat node.y
                    ]
                    [ Svg.text text ]
                ]

        toPointsString : List Point -> String
        toPointsString points =
            List.map (\{ x, y } -> String.fromFloat x ++ "," ++ String.fromFloat y) points
                |> List.foldl (\pointString acc -> acc ++ " " ++ pointString) ""

        edgeToPolyline : Edge -> Svg msg
        edgeToPolyline =
            \points ->
                Svg.polyline
                    [ SvgA.points <| toPointsString points
                    , SvgA.stroke "teal"
                    , SvgA.fill "none"
                    ]
                    []

        dataToPointsString : List OperatorDatum -> String
        dataToPointsString data =
            let
                ( tss, vals ) =
                    List.unzip data

                minTs =
                    List.minimum tss |> Maybe.withDefault 0

                maxTs =
                    List.maximum tss |> Maybe.withDefault 0

                minVal =
                    List.minimum vals |> Maybe.withDefault 0

                maxVal =
                    List.minimum vals |> Maybe.withDefault 0
            in
            List.map
                (\( ts, val ) ->
                    let
                        normedX =
                            (toFloat ts - toFloat minTs) / (toFloat maxTs - toFloat minTs)

                        normedY =
                            (toFloat val - toFloat minVal) / (toFloat maxVal - toFloat minVal)
                    in
                    (String.fromFloat <| normedX * 800)
                        ++ ","
                        ++ (String.fromFloat <| normedY * 600)
                )
                data
                |> List.foldl (\pointString acc -> acc ++ " " ++ pointString) ""

        operatorDataToPolyline : List OperatorDatum -> List (Svg msg)
        operatorDataToPolyline data =
            [ Svg.polyline
                [ SvgA.points <| dataToPointsString data
                , SvgA.stroke "red"
                , SvgA.fill "none"
                ]
                []
            ]
    in
    Svg.svg
        [ SvgA.width "800"
        , SvgA.height "600"
        ]
        (List.concat
            [ List.concatMap toRect <| NodeDict.positionedNodes model.nodeDict
            , List.map edgeToPolyline model.edges
            , List.concatMap operatorDataToPolyline <| Dict.values model.operatorDataDict
            ]
        )
