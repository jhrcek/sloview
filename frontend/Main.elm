module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task
import SLModel exposing (..)
import Time exposing (Time)
import String


main : Program Never Model Msg
main =
    Html.program
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
    , enabledMessageFields : List SLMessageField
    }


type Msg
    = LoadSuccess String
    | LoadFailed Http.Error
    | LogLevelChange LogLevel Bool
    | MessageFieldChange SLMessageField Bool


init : ( Model, Cmd Msg )
init =
    ( { serverLog = []
      , logLevelCounts = ( 0, 0, 0, 0, 0, 0 )
      , notifications = [ "Initializing ..." ]
      , enabledLogLevels = [ FATAL, ERROR, WARN ]
      , enabledMessageFields = [ TimeField, PayloadField ]
      }
    , loadServerLog
    )


loadServerLog : Cmd Msg
loadServerLog =
    let
        procResult res =
            case res of
                Err e ->
                    LoadFailed e

                Ok r ->
                    LoadSuccess r
    in
        Http.toTask (Http.getString "/static/server.log")
            |> Task.attempt procResult


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

        MessageFieldChange field isEnabled ->
            { model
                | enabledMessageFields =
                    if isEnabled then
                        field :: model.enabledMessageFields
                    else
                        List.filter ((/=) field) model.enabledMessageFields
            }
                ! []


view : Model -> Html Msg
view ({ serverLog, notifications, enabledLogLevels, enabledMessageFields } as model) =
    div []
        [ filterControls model
        , notificationsView notifications
        , serverLog
            |> List.filter (\(SLMessage _ _ logLevel _ _ _) -> List.member logLevel enabledLogLevels)
            |> List.map (viewMessage enabledMessageFields)
            |> div []
        ]


notificationsView : List String -> Html Msg
notificationsView errMsgs =
    div [ style [ ( "color", "red" ) ] ] <|
        List.map (\m -> div [] [ text m ]) errMsgs


viewMessage : List SLMessageField -> SLMessage -> Html Msg
viewMessage enabledFields (SLMessage date time logLevel logger thread payload) =
    let
        includeField field value =
            if List.member field enabledFields then
                value
            else
                ""
    in
        div [ logLevelClass logLevel ]
            [ text <|
                String.join " "
                    [ includeField TimeField (formatTime time)
                    , includeField LogLevelField (toString logLevel)
                    , includeField LoggerField ("[" ++ logger ++ "]")
                    , includeField ThreadField ("(" ++ thread ++ ")")
                    , includeField PayloadField payload
                    ]
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
filterControls { serverLog, logLevelCounts, enabledLogLevels, enabledMessageFields } =
    div [ class "controls" ] <|
        logLevelCheckboxes enabledLogLevels logLevelCounts
            ++ messageFieldChecboxes enabledMessageFields


logLevelCheckboxes : List LogLevel -> ( Int, Int, Int, Int, Int, Int ) -> List (Html Msg)
logLevelCheckboxes enabledLogLevels logLevelCounts =
    let
        logLevelCheckbox levelCount logLevel =
            div [ logLevelClass logLevel ]
                [ input [ type_ "checkbox", checked (List.member logLevel enabledLogLevels), onCheck (LogLevelChange logLevel) ] []
                , text <| toString logLevel ++ " (" ++ toString levelCount ++ ")"
                ]

        ( f, e, w, i, d, t ) =
            logLevelCounts
    in
        [ h4 [] [ text "Log Level (# of msgs)" ]
        , logLevelCheckbox f FATAL
        , logLevelCheckbox e ERROR
        , logLevelCheckbox w WARN
        , logLevelCheckbox i INFO
        , logLevelCheckbox d DEBUG
        , logLevelCheckbox t TRACE
        ]


messageFieldChecboxes : List SLMessageField -> List (Html Msg)
messageFieldChecboxes enabledMessageFields =
    let
        messageFieldCheckbox field =
            div []
                [ input [ type_ "checkbox", checked (List.member field enabledMessageFields), onCheck (MessageFieldChange field) ] []
                , text <| String.dropRight 5 <| toString field
                ]
    in
        [ h4 [] [ text "Message fields" ]
        , messageFieldCheckbox TimeField
        , messageFieldCheckbox LogLevelField
        , messageFieldCheckbox LoggerField
        , messageFieldCheckbox ThreadField
        , messageFieldCheckbox PayloadField
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
