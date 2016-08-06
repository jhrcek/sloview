module Main exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import SLModel exposing (..)
import Time exposing (Time)
import String


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { serverLog : ServerLog
    , logLevelCounts : ( Int, Int, Int, Int, Int, Int )
    , error : Maybe String
    , enabledLogLevels : List LogLevel
    }


type Msg
    = LoadSuccess String
    | LoadFailed Http.Error
    | LogLevelChange LogLevel Bool


init : ( Model, Cmd Msg )
init =
    ( { serverLog = []
      , logLevelCounts = ( 0, 0, 0, 0, 0, 0 )
      , error = Just "Initializing ..."
      , enabledLogLevels = [ FATAL, ERROR, WARN ]
      }
    , loadServerLog
    )


loadServerLog : Cmd Msg
loadServerLog =
    Task.perform LoadFailed LoadSuccess (Http.getString "http://0.0.0.0:8000/static/server.log")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadSuccess slogText ->
            case SLModel.parseServerLog slogText of
                Ok slog ->
                    { model | serverLog = slog, logLevelCounts = countLevels slog, error = Nothing } ! []

                Err err ->
                    { model | serverLog = [], error = Just err } ! []

        LoadFailed err ->
            { model | serverLog = [], error = Just (toString err) } ! []

        LogLevelChange level isEnabled ->
            { model
                | enabledLogLevels =
                    if isEnabled then
                        level :: model.enabledLogLevels
                    else
                        List.filter ((/=) level) model.enabledLogLevels
            }
                ! []


view : Model -> Html Msg
view ({ serverLog, error, enabledLogLevels } as model) =
    div []
        [ filterControls model
        , div [ style [ ( "color", "red" ) ] ] [ text <| Maybe.withDefault "" error ]
        , serverLog
            |> List.filter (\(SLMessage _ logLevel _ _ _) -> List.member logLevel enabledLogLevels)
            |> List.map viewMessage
            |> div [ style [ ( "white-space", "pre-wrap" ) ] ]
        ]


viewMessage : SLMessage -> Html Msg
viewMessage (SLMessage time logLevel logger thread payload) =
    div (logLevelStyle logLevel)
        [ text <| String.join " " [ formatTime time {- , toString logLevel -}, payload ]
        , hr [ style [ ( "margin", "0" ), ( "background", "#777777" ), ( "border", "0" ), ( "height", "1px" ) ] ] []
        ]


formatTime : Time -> String
formatTime totalMillis =
    let
        hrs =
            floor <| Time.inHours totalMillis

        mins =
            floor << Time.inMinutes <| totalMillis - toFloat hrs * Time.hour

        secs =
            floor << Time.inSeconds <| totalMillis - toFloat hrs * Time.hour - toFloat mins * Time.minute

        twoDigits =
            String.padLeft 2 '0' << toString
    in
        String.join ":"
            [ twoDigits hrs
            , twoDigits mins
            , twoDigits secs
            ]


filterControls : Model -> Html Msg
filterControls { serverLog, enabledLogLevels, logLevelCounts } =
    let
        checkbox levelCount logLevel =
            div (logLevelStyle logLevel)
                [ input [ type' "checkbox", checked (List.member logLevel enabledLogLevels), onCheck (LogLevelChange logLevel) ] []
                , text <| toString logLevel ++ " (" ++ toString levelCount ++ ")"
                ]

        ( f, e, w, i, d, t ) =
            logLevelCounts
    in
        div
            [ style
                [ ( "display", "block" )
                , ( "position", "fixed" )
                , ( "right", "0" )
                , ( "top", "0" )
                , ( "padding", "0" )
                , ( "max-width", "75%" )
                , ( "background-color", "white" )
                ]
            ]
            [ h4 [ style [ ( "margin", "0" ) ] ] [ text "Log Level (# of msgs)" ]
            , checkbox f FATAL
            , checkbox e ERROR
            , checkbox w WARN
            , checkbox i INFO
            , checkbox d DEBUG
            , checkbox t TRACE
            ]


countLevels : ServerLog -> ( Int, Int, Int, Int, Int, Int )
countLevels slog =
    let
        addLevel lvl ( f, e, w, i, d, t ) =
            case lvl of
                FATAL ->
                    ( f + 1, e, w, i, d, t )

                ERROR ->
                    ( f, e + 1, w, i, d, t )

                WARN ->
                    ( f, e, w + 1, i, d, t )

                INFO ->
                    ( f, e, w, i + 1, d, t )

                DEBUG ->
                    ( f, e, w, i, d + 1, t )

                TRACE ->
                    ( f, e, w, i, d, t + 1 )
    in
        List.foldl (\(SLMessage _ lvl _ _ _) counts -> addLevel lvl counts) ( 0, 0, 0, 0, 0, 0 ) slog


logLevelStyle : LogLevel -> List (Attribute Msg)
logLevelStyle ll =
    let
        bgColor c =
            [ style [ ( "background-color", c ) ] ]
    in
        case ll of
            FATAL ->
                bgColor "#FF2626"

            ERROR ->
                bgColor "#FFA04A"

            WARN ->
                bgColor "#FFF284"

            -- leave INFO white, because it's the most common
            INFO ->
                []

            DEBUG ->
                bgColor "#CAFFD8"

            TRACE ->
                bgColor "#C0F7FE"
