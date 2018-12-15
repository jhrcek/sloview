module Main exposing (main)

import Browser
import DateFormat
import Html exposing (Attribute, Html, br, div, h4, hr, input, label, option, select, text)
import Html.Attributes exposing (action, checked, class, enctype, method, name, style, type_, value)
import Html.Events exposing (on, onCheck, onClick)
import Json.Decode as Json
import Model.ServerLog
    exposing
        ( AggregatedMessageByPayload(..)
        , LogLevel(..)
        , ServerLog
        , ServerLogMessage(..)
        , ServerLogMessageField(..)
        , aggregateMessagesByPayload
        , fieldToString
        , logLevelToString
        , serverLogDecoder
        )
import Time


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { serverLog : ServerLog
    , logLevelCounts : LogLevelCounts
    , notifications : List String
    , enabledLogLevels : List LogLevel
    , enabledMessageFields : List ServerLogMessageField
    , dateFormat : DateFormatConfig
    , viewMode : ViewMode
    }


type alias DateFormatConfig =
    List DateFormat.Token


type alias LogLevelCounts =
    { fatal : Int
    , error : Int
    , warn : Int
    , info : Int
    , debug : Int
    , trace : Int
    , unknown : Int
    }


type Msg
    = LogLevelChange LogLevel Bool
    | MessageFieldChange ServerLogMessageField Bool
    | DateFormatChange DateFormatConfig
    | ModeChange ViewMode


type ViewMode
    = MessagesMode
    | AggregatedMessagesMode


init : String -> ( Model, Cmd Msg )
init serverLogJson =
    let
        ( sl, errs ) =
            parseServerLogJson serverLogJson
    in
    ( { serverLog = sl
      , logLevelCounts = countLevels sl
      , notifications = errs
      , enabledLogLevels = [ FATAL, ERROR, WARN ]
      , enabledMessageFields = [ DateTimeField, PayloadField ]
      , dateFormat = timeOnlyFormat
      , viewMode = MessagesMode
      }
    , Cmd.none
    )


parseServerLogJson : String -> ( ServerLog, List String )
parseServerLogJson serverLogJson =
    case Json.decodeString serverLogDecoder serverLogJson of
        Ok slog ->
            ( slog, [] )

        Err err ->
            ( [], [ Json.errorToString err ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogLevelChange level isEnabled ->
            ( { model | enabledLogLevels = updateList level isEnabled model.enabledLogLevels }
            , Cmd.none
            )

        MessageFieldChange field isEnabled ->
            ( { model | enabledMessageFields = updateList field isEnabled model.enabledMessageFields }
            , Cmd.none
            )

        DateFormatChange df ->
            ( { model | dateFormat = df }, Cmd.none )

        ModeChange m ->
            ( { model | viewMode = m }, Cmd.none )


updateList : a -> Bool -> List a -> List a
updateList item addItem list =
    if addItem then
        item :: list

    else
        List.filter ((/=) item) list


view : Model -> Html Msg
view model =
    let
        viewForMode =
            case model.viewMode of
                MessagesMode ->
                    messagesView model

                AggregatedMessagesMode ->
                    aggregatedMessagesView model
    in
    div []
        [ controlsPanel model
        , notificationsView model.notifications
        , viewForMode
        ]


messagesView : Model -> Html Msg
messagesView m =
    let
        messagesToDisplay =
            m.serverLog
                |> List.filter (\(M _ logLevel _ _ _ _) -> List.member logLevel m.enabledLogLevels)
                |> List.map (viewMessage m.dateFormat m.enabledMessageFields)
    in
    if List.isEmpty m.serverLog then
        div [] [ text noServerLog ]

    else if List.isEmpty m.enabledMessageFields then
        div [] [ text noFieldsEnabled ]

    else if List.isEmpty messagesToDisplay then
        div [] [ text noMessagesToDisplay ]

    else
        div [] messagesToDisplay


noFieldsEnabled : String
noFieldsEnabled =
    "Nothing to show - no message fields are enabled. You can enable some using the controls on the right"


noServerLog : String
noServerLog =
    "No data available. You can upload server.log using controls on the right"


noMessagesToDisplay : String
noMessagesToDisplay =
    "No messages match specified criteria."


viewMessage : DateFormatConfig -> List ServerLogMessageField -> ServerLogMessage -> Html Msg
viewMessage dateFormat enabledFields (M date logLevel logger thread payload stacktrace) =
    let
        includeField field value =
            if List.member field enabledFields then
                [ value ]

            else
                []
    in
    div [ logLevelClass logLevel ]
        [ text <|
            String.join " " <|
                List.concat
                    [ includeField DateTimeField (DateFormat.format dateFormat Time.utc date)
                    , includeField LogLevelField (logLevelToString logLevel)
                    , includeField LoggerField ("[" ++ logger ++ "]")
                    , includeField ThreadField ("(" ++ thread ++ ")")
                    , includeField PayloadField payload
                    , includeField StacktraceField (Maybe.withDefault "" stacktrace)
                    ]
        , hr [] []
        ]


aggregatedMessagesView : Model -> Html Msg
aggregatedMessagesView m =
    let
        messagesToDisplay =
            m.serverLog
                |> aggregateMessagesByPayload
                |> List.filter (\(AMP _ logLevel _) -> List.member logLevel m.enabledLogLevels)
                |> List.map viewAggregatedMessage
    in
    if List.isEmpty m.serverLog then
        div [] [ text noServerLog ]

    else if List.isEmpty messagesToDisplay then
        div [] [ text noMessagesToDisplay ]

    else
        div [] messagesToDisplay


viewAggregatedMessage : AggregatedMessageByPayload -> Html Msg
viewAggregatedMessage (AMP payload logLevel count) =
    div [ logLevelClass logLevel ]
        [ text <| String.fromInt count ++ "x " ++ payload
        , hr [] []
        ]


uploadForm : Html Msg
uploadForm =
    Html.form [ enctype "multipart/form-data", action "doUpload", method "POST" ]
        [ input [ name "file", type_ "file" ] []
        , br [] []
        , input [ type_ "submit", value "Upload server.log" ] []
        ]


notificationsView : List String -> Html Msg
notificationsView errMsgs =
    div [ style "color" "red" ] <|
        List.map (\m -> div [] [ text m ]) errMsgs


controlsPanel : Model -> Html Msg
controlsPanel { logLevelCounts, enabledLogLevels, enabledMessageFields, viewMode } =
    let
        controlsRelevantForMessageView =
            if viewMode == MessagesMode then
                messageFieldChecboxes enabledMessageFields
                    ++ [ dateFormatSelect ]

            else
                []
    in
    div [ class "controls" ] <|
        uploadForm
            :: viewModeRadios viewMode
            :: logLevelCheckboxes enabledLogLevels logLevelCounts
            ++ controlsRelevantForMessageView


logLevelCheckboxes : List LogLevel -> LogLevelCounts -> List (Html Msg)
logLevelCheckboxes enabledLogLevels logCounts =
    let
        logLevelCheckbox levelCount logLevel =
            div [ logLevelClass logLevel ]
                [ input [ type_ "checkbox", checked (List.member logLevel enabledLogLevels), onCheck (LogLevelChange logLevel) ] []
                , text <| logLevelToString logLevel ++ " (" ++ String.fromInt levelCount ++ ")"
                ]
    in
    [ h4 [] [ text "Log Level (# of msgs)" ]
    , logLevelCheckbox logCounts.fatal FATAL
    , logLevelCheckbox logCounts.error ERROR
    , logLevelCheckbox logCounts.warn WARN
    , logLevelCheckbox logCounts.info INFO
    , logLevelCheckbox logCounts.debug DEBUG
    , logLevelCheckbox logCounts.trace TRACE
    , logLevelCheckbox logCounts.unknown UNKNOWN
    ]


messageFieldChecboxes : List ServerLogMessageField -> List (Html Msg)
messageFieldChecboxes enabledMessageFields =
    let
        messageFieldCheckbox field =
            div []
                [ input [ type_ "checkbox", checked (List.member field enabledMessageFields), onCheck (MessageFieldChange field) ] []
                , text <| fieldToString field
                ]
    in
    [ h4 [] [ text "Message fields" ]
    , messageFieldCheckbox DateTimeField
    , messageFieldCheckbox LogLevelField
    , messageFieldCheckbox LoggerField
    , messageFieldCheckbox ThreadField
    , messageFieldCheckbox PayloadField
    , messageFieldCheckbox StacktraceField
    ]


dateFormatSelect : Html Msg
dateFormatSelect =
    let
        availableOptions =
            [ ( "1", "HH:MM:SS" )
            , ( "2", "YYYY-mm-dd HH:MM:SS" )
            ]

        optionDecoder =
            Json.at [ "target", "value" ] Json.string
                |> Json.andThen
                    (\str ->
                        case str of
                            "1" ->
                                Json.succeed timeOnlyFormat

                            "2" ->
                                Json.succeed dateAndTimeFormat

                            _ ->
                                Json.fail <| "invalid string" ++ str
                    )

        formatOption ( val, label ) =
            option [ value val ] [ text label ]
    in
    div []
        [ h4 [] [ text "Date format" ]
        , select [ on "change" (Json.map DateFormatChange optionDecoder) ]
            (List.map formatOption availableOptions)
        ]


dateAndTimeFormat : DateFormatConfig
dateAndTimeFormat =
    [ DateFormat.yearNumber
    , DateFormat.text "-"
    , DateFormat.monthFixed
    , DateFormat.text "-"
    , DateFormat.dayOfMonthFixed
    , DateFormat.text " "
    ]
        ++ timeOnlyFormat


timeOnlyFormat : DateFormatConfig
timeOnlyFormat =
    [ DateFormat.hourMilitaryFixed
    , DateFormat.text ":"
    , DateFormat.minuteFixed
    , DateFormat.text ":"
    , DateFormat.secondFixed
    ]


viewModeRadios : ViewMode -> Html Msg
viewModeRadios currentMode =
    div []
        [ h4 [] [ text "View mode" ]
        , radio "Messages" (ModeChange MessagesMode) (currentMode == MessagesMode)
        , radio "Aggregated Messages" (ModeChange AggregatedMessagesMode) (currentMode == AggregatedMessagesMode)
        ]


radio : String -> msg -> Bool -> Html msg
radio lbl msg isChecked =
    label []
        [ input [ type_ "radio", name "viewMode", onClick msg, checked isChecked ] []
        , text lbl
        ]


countLevels : ServerLog -> LogLevelCounts
countLevels slog =
    let
        addLevel lvl counts =
            case lvl of
                FATAL ->
                    { counts | fatal = counts.fatal + 1 }

                ERROR ->
                    { counts | error = counts.error + 1 }

                WARN ->
                    { counts | warn = counts.warn + 1 }

                INFO ->
                    { counts | info = counts.info + 1 }

                DEBUG ->
                    { counts | debug = counts.debug + 1 }

                TRACE ->
                    { counts | trace = counts.trace + 1 }

                UNKNOWN ->
                    { counts | unknown = counts.unknown + 1 }
    in
    List.foldl (\(M _ lvl _ _ _ _) counts -> addLevel lvl counts) (LogLevelCounts 0 0 0 0 0 0 0) slog


logLevelClass : LogLevel -> Attribute Msg
logLevelClass =
    class << String.toLower << logLevelToString
