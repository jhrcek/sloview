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
    , notifications : List String
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
      , notifications = [ "Initializing ..." ]
      , enabledLogLevels = [ FATAL, ERROR, WARN ]
      }
    , loadServerLog
    )


loadServerLog : Cmd Msg
loadServerLog =
    Task.perform LoadFailed LoadSuccess (Http.getString "/static/server.log")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadSuccess slogText ->
            let
                ( unparsedMessages, parsedMessagesList ) =
                    SLModel.parseServerLog slogText
            in
                { model
                    | serverLog = parsedMessagesList
                    , logLevelCounts = countLevels parsedMessagesList
                    , notifications = unparsedMessages
                }
                    ! []

        LoadFailed err ->
            { model | serverLog = [], notifications = [ toString err ] } ! []

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
view ({ serverLog, notifications, enabledLogLevels } as model) =
    div []
        [ filterControls model
        , notificationsView notifications
        , serverLog
            |> List.filter (\(SLMessage _ _ logLevel _ _ _) -> List.member logLevel enabledLogLevels)
            |> List.map viewMessage
            |> div []
        ]


notificationsView : List String -> Html Msg
notificationsView errMsgs =
    div [ style [ ( "color", "red" ) ] ]
        <| List.map (\m -> div [] [ text m ]) errMsgs


viewMessage : SLMessage -> Html Msg
viewMessage (SLMessage date time logLevel logger thread payload) =
    div [ logLevelClass logLevel ]
        [ text <| String.join " " [ formatTime time {- , toString logLevel -}, payload ]
        , hr [] []
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
            div [ logLevelClass logLevel ]
                [ input [ type' "checkbox", checked (List.member logLevel enabledLogLevels), onCheck (LogLevelChange logLevel) ] []
                , text <| toString logLevel ++ " (" ++ toString levelCount ++ ")"
                ]

        ( f, e, w, i, d, t ) =
            logLevelCounts
    in
        div [ class "controls" ]
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
        List.foldl (\(SLMessage _ _ lvl _ _ _) counts -> addLevel lvl counts) ( 0, 0, 0, 0, 0, 0 ) slog


logLevelClass : LogLevel -> Attribute Msg
logLevelClass =
    class << String.toLower << toString
