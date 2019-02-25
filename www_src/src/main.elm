port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type alias Model =
    { responses : List String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { responses = [] }, Cmd.none )


type Msg
    = Echo String


subscriptions : Model -> Sub Msg
subscriptions model =
    wsEcho Echo


port wsEcho : (String -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Echo value ->
            ( { model | responses = value :: model.responses }, Cmd.none )


li : String -> Html msg
li string =
    Html.li [] [ Html.text string ]


view : Model -> Html msg
view model =
    model.responses |> List.map li |> Html.ul []
