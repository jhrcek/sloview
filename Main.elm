module Main exposing (..)

import Html exposing (..)
import Html.App
import Http
import Task


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    String


type Msg
    = LoadSuccess String
    | LoadFailed Http.Error


init : ( Model, Cmd Msg )
init =
    ( "Initializing ..", loadServerLog )


loadServerLog : Cmd Msg
loadServerLog =
    Task.perform LoadFailed LoadSuccess (Http.getString "http://0.0.0.0:8000/static/server.log")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadSuccess slog ->
            slog ! []

        LoadFailed err ->
            toString err ! []


view : Model -> Html Msg
view model =
    text model
