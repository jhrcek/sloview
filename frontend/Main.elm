module Main exposing (..)

import Date.Format
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Model.ServerLog exposing (..)
import Time exposing (Time)


main : Program String Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { serverLog : ServerLog
    , logLevelCounts : ( Int, Int, Int, Int, Int, Int, Int )
    , notifications : List String
    , enabledLogLevels : List LogLevel
    , enabledMessageFields : List ServerLogMessageField
    }


type Msg
    = LogLevelChange LogLevel Bool
    | MessageFieldChange ServerLogMessageField Bool


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
          }
        , Cmd.none
        )


parseServerLogJson : String -> ( ServerLog, List String )
parseServerLogJson serverLogJson =
    case Json.Decode.decodeString serverLogDecoder serverLogJson of
        Ok slog ->
            ( slog, [] )

        Err err ->
            ( [], [ err ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogLevelChange level isEnabled ->
            ( { model
                | enabledLogLevels =
                    if isEnabled then
                        level :: model.enabledLogLevels
                    else
                        List.filter ((/=) level) model.enabledLogLevels
              }
            , Cmd.none
            )

        MessageFieldChange field isEnabled ->
            ( { model
                | enabledMessageFields =
                    if isEnabled then
                        field :: model.enabledMessageFields
                    else
                        List.filter ((/=) field) model.enabledMessageFields
              }
            , Cmd.none
            )


view : Model -> Html Msg
view ({ serverLog, notifications, enabledLogLevels, enabledMessageFields } as model) =
    let
        messagesView =
            if List.isEmpty enabledMessageFields then
                div [] []
            else
                serverLog
                    |> List.filter (\(M _ logLevel _ _ _ _) -> List.member logLevel enabledLogLevels)
                    |> List.map (viewMessage enabledMessageFields)
                    |> div []
    in
        div []
            [ controlsPanel model
            , notificationsView notifications
            , messagesView
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
    div [ style [ ( "color", "red" ) ] ] <|
        List.map (\m -> div [] [ text m ]) errMsgs


viewMessage : List ServerLogMessageField -> ServerLogMessage -> Html Msg
viewMessage enabledFields (M date logLevel logger thread payload exception) =
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
                    [ includeField DateTimeField (Date.Format.format "%Y-%m-%d %H:%M:%S" date)
                    , includeField LogLevelField (toString logLevel)
                    , includeField LoggerField ("[" ++ logger ++ "]")
                    , includeField ThreadField ("(" ++ thread ++ ")")
                    , includeField PayloadField payload
                    , includeField ExceptionField (Maybe.withDefault "" exception)
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


controlsPanel : Model -> Html Msg
controlsPanel { serverLog, logLevelCounts, enabledLogLevels, enabledMessageFields } =
    div [ class "controls" ] <|
        uploadForm
            :: logLevelCheckboxes enabledLogLevels logLevelCounts
            ++ messageFieldChecboxes enabledMessageFields


logLevelCheckboxes : List LogLevel -> ( Int, Int, Int, Int, Int, Int, Int ) -> List (Html Msg)
logLevelCheckboxes enabledLogLevels logLevelCounts =
    let
        logLevelCheckbox levelCount logLevel =
            div [ logLevelClass logLevel ]
                [ input [ type_ "checkbox", checked (List.member logLevel enabledLogLevels), onCheck (LogLevelChange logLevel) ] []
                , text <| toString logLevel ++ " (" ++ toString levelCount ++ ")"
                ]

        ( f, e, w, i, d, t, u ) =
            logLevelCounts
    in
        [ h4 [] [ text "Log Level (# of msgs)" ]
        , logLevelCheckbox f FATAL
        , logLevelCheckbox e ERROR
        , logLevelCheckbox w WARN
        , logLevelCheckbox i INFO
        , logLevelCheckbox d DEBUG
        , logLevelCheckbox t TRACE
        , logLevelCheckbox u UNKNOWN
        ]


messageFieldChecboxes : List ServerLogMessageField -> List (Html Msg)
messageFieldChecboxes enabledMessageFields =
    let
        messageFieldCheckbox field =
            div []
                [ input [ type_ "checkbox", checked (List.member field enabledMessageFields), onCheck (MessageFieldChange field) ] []
                , text <| String.dropRight 5 <| toString field
                ]
    in
        [ h4 [] [ text "Message fields" ]
        , messageFieldCheckbox DateTimeField
        , messageFieldCheckbox LogLevelField
        , messageFieldCheckbox LoggerField
        , messageFieldCheckbox ThreadField
        , messageFieldCheckbox PayloadField
        , messageFieldCheckbox ExceptionField
        ]


countLevels : ServerLog -> ( Int, Int, Int, Int, Int, Int, Int )
countLevels slog =
    let
        addLevel lvl ( f, e, w, i, d, t, u ) =
            case lvl of
                FATAL ->
                    ( f + 1, e, w, i, d, t, u )

                ERROR ->
                    ( f, e + 1, w, i, d, t, u )

                WARN ->
                    ( f, e, w + 1, i, d, t, u )

                INFO ->
                    ( f, e, w, i + 1, d, t, u )

                DEBUG ->
                    ( f, e, w, i, d + 1, t, u )

                TRACE ->
                    ( f, e, w, i, d, t + 1, u )

                UNKNOWN ->
                    ( f, e, w, i, d, t, u + 1 )
    in
        List.foldl (\(M _ lvl _ _ _ _) counts -> addLevel lvl counts) ( 0, 0, 0, 0, 0, 0, 0 ) slog


logLevelClass : LogLevel -> Attribute Msg
logLevelClass =
    class << String.toLower << toString
