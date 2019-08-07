module NodeDict exposing (NodeDict, PositionedNode, SizedNode, attachDatum, dropData, empty, encodeNodesToBePositioned, insertNode, positionNode, positionedNodeDecoder, positionedNodes, singleton, sizedNodes)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListExtra


type alias Datum =
    ( Int, Int )


type alias NodeWithData a =
    { a | data : Maybe (List Datum) }


type alias SizedNode =
    { height : Float, width : Float, data : Maybe (List Datum) }


type alias PositionedNode =
    { height : Float, width : Float, data : Maybe (List Datum), x : Float, y : Float }


encodeSizedNode : SizedNode -> Encode.Value
encodeSizedNode { height, width } =
    Encode.object
        [ ( "height", Encode.float height )
        , ( "width", Encode.float width )
        ]


encodePositionedNode : PositionedNode -> Encode.Value
encodePositionedNode { height, width, x, y } =
    Encode.object
        [ ( "x", Encode.float x )
        , ( "y", Encode.float y )
        , ( "height", Encode.float height )
        , ( "width", Encode.float width )
        ]


type alias PositionedNodeDict =
    Dict String PositionedNode


type alias SizedNodeDict =
    Dict String SizedNode


type NodeDict
    = NodeDict SizedNodeDict PositionedNodeDict


empty : NodeDict
empty =
    NodeDict Dict.empty Dict.empty


singleton : String -> SizedNode -> NodeDict
singleton key sizedNode =
    NodeDict (Dict.singleton key sizedNode) Dict.empty


insertNode : String -> SizedNode -> NodeDict -> NodeDict
insertNode key sizedNode nodeDict =
    case nodeDict of
        NodeDict sizedNodeDict positionedNodeDict ->
            case Dict.member key positionedNodeDict of
                True ->
                    nodeDict

                False ->
                    NodeDict (Dict.insert key sizedNode sizedNodeDict) positionedNodeDict


positionNode : String -> PositionedNode -> NodeDict -> NodeDict
positionNode key positionedNode nodeDict =
    case nodeDict of
        NodeDict sizedNodeDict positionedNodeDict ->
            NodeDict (Dict.remove key sizedNodeDict) (Dict.insert key positionedNode positionedNodeDict)


upsertDatum : Datum -> Maybe (List Datum) -> Maybe (List Datum)
upsertDatum datum mData =
    case mData of
        Just data ->
            Just (data ++ [ datum ])

        Nothing ->
            Just (List.singleton datum)


attachDatum : String -> Datum -> NodeDict -> NodeDict
attachDatum key datum nodeDict =
    let
        upsert : Maybe (NodeWithData a) -> Maybe (NodeWithData a)
        upsert mNode =
            case mNode of
                Just node ->
                    Just { node | data = upsertDatum datum node.data }

                Nothing ->
                    Nothing
    in
    case nodeDict of
        NodeDict sizedNodeDict positionedNodeDict ->
            NodeDict (Dict.update key upsert sizedNodeDict) (Dict.update key upsert positionedNodeDict)


dropData : Int -> NodeDict -> NodeDict
dropData time nodeDict =
    let
        oldDatum : Datum -> Bool
        oldDatum ( ts, v ) =
            ts < time

        dropper : String -> NodeWithData a -> NodeWithData a
        dropper _ node =
            { node | data = Maybe.map (ListExtra.dropWhile oldDatum) node.data }
    in
    case nodeDict of
        NodeDict sizedNodeDict positionedNodeDict ->
            NodeDict (Dict.map dropper sizedNodeDict) (Dict.map dropper positionedNodeDict)


union : NodeDict -> SizedNodeDict
union nodeDict =
    case nodeDict of
        NodeDict sizedDict positionedDict ->
            Dict.union
                sizedDict
                (Dict.map (\k { height, width, data } -> { height = height, width = width, data = data }) positionedDict)


encodeNodesToBePositioned : NodeDict -> Maybe Encode.Value
encodeNodesToBePositioned nodeDict =
    case nodeDict of
        NodeDict sizedNodeDict positionedNodeDict ->
            case Dict.isEmpty sizedNodeDict of
                True ->
                    Nothing

                False ->
                    Just <| Encode.dict identity encodeSizedNode <| union nodeDict


positionedNodeDecoder : Decoder PositionedNode
positionedNodeDecoder =
    Decode.map5 PositionedNode
        (Decode.field "height" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.succeed Nothing)
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


sizedNodes : NodeDict -> List ( String, SizedNode )
sizedNodes nodeDict =
    case nodeDict of
        NodeDict sizedDict _ ->
            Dict.toList sizedDict


positionedNodes : NodeDict -> List ( String, PositionedNode )
positionedNodes nodeDict =
    case nodeDict of
        NodeDict _ positionedDict ->
            Dict.toList positionedDict
