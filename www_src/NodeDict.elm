module NodeDict exposing (NodeDict, PositionedNode, SizedNode, empty, encodeNodesToBePositioned, insertNode, positionNode, positionedNodeDecoder, positionedNodes, singleton, sizedNodes)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias SizedNode =
    { height : Float, width : Float }


type alias PositionedNode =
    { height : Float, width : Float, x : Float, y : Float }


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


union : NodeDict -> SizedNodeDict
union nodeDict =
    case nodeDict of
        NodeDict sizedDict positionedDict ->
            Dict.union
                sizedDict
                (Dict.map (\k { height, width } -> { height = height, width = width }) positionedDict)


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
    Decode.map4 PositionedNode
        (Decode.field "height" Decode.float)
        (Decode.field "width" Decode.float)
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
