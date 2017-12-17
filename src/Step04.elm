module Main exposing (main)

import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Task
import Time


type alias Model =
    { error : String
    }


type Msg
    = MessageWithCommand
    | MessageWithoutCommand
    | Command Time.Time
    | None


init : ( Model, Cmd msg )
init =
    { error = "" } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        MessageWithCommand ->
            ( { model | error = "Command not called yet" }
            , Task.perform Command Time.now
            )

        MessageWithoutCommand ->
            ( { model | error = "Command not called yet" }
            , Cmd.none
            )

        Command time ->
            -- Command called, cancelling "Command not called yet"
            ( { model | error = "" }
            , Cmd.none
            )

        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Events.onClick MessageWithCommand ]
            [ Html.text "Message With Command" ]
        , Html.button [ Events.onClick MessageWithoutCommand ]
            [ Html.text "Message Without Command" ]
        , Html.button [ Events.onClick (Command 0) ]
            [ Html.text "Command" ]
        , Html.div [ Attr.style [ ( "color", "red" ) ] ]
            [ Html.text model.error ]
        ]


encodeMsg : Msg -> Value
encodeMsg msg =
    case msg of
        MessageWithCommand ->
            Encode.string "MessageWithCommand"

        MessageWithoutCommand ->
            Encode.string "MessageWithoutCommand"

        Command time ->
            Encode.string "Command"

        None ->
            Encode.string "None"


decodeMsg : Decoder Msg
decodeMsg =
    Decode.string
        |> Decode.andThen
            (\msgString ->
                case msgString of
                    "MessageWithoutCommand" ->
                        Decode.succeed MessageWithoutCommand

                    "MessageWithCommand" ->
                        Decode.succeed MessageWithCommand

                    "Command" ->
                        Decode.succeed None

                    "None" ->
                        Decode.succeed None

                    _ ->
                        Decode.fail "unknown msg type"
            )


main : QAProgram Never Model Msg
main =
    qaProgram
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



---


type alias WrappedModel model msg =
    { wrapped : model
    , initial : model
    , messages : List msg
    , input : String
    }


wrapperInit :
    ( model, Cmd msg )
    -> ( WrappedModel model msg, Cmd (WrappedMsg msg) )
wrapperInit ( model, msgs ) =
    ( { wrapped = model
      , initial = model
      , messages = []
      , input = """[
  "MessageWithCommand"
]"""
      }
    , Cmd.map Wrapped msgs
    )


type WrappedMsg msg
    = Wrapped msg
    | Update String
    | Replay


wrapperUpdate :
    (Msg -> model -> ( model, Cmd msg ))
    -> WrappedMsg Msg
    -> WrappedModel model Msg
    -> ( WrappedModel model Msg, Cmd (WrappedMsg msg) )
wrapperUpdate wrappedUpdate msg model =
    case Debug.log "\nwrapperMsg" msg of
        Wrapped wrappedMsg ->
            let
                ( wrapped, msgs ) =
                    wrappedUpdate wrappedMsg model.wrapped
            in
            ( { model | wrapped = wrapped, messages = wrappedMsg :: model.messages }
            , Cmd.map Wrapped msgs
            )

        Update input ->
            ( { model | input = input }, Cmd.none )

        Replay ->
            case Decode.decodeString (Decode.list decodeMsg) model.input of
                Ok msgs ->
                    ( { model
                        | wrapped = List.foldl (\msg model -> wrappedUpdate msg model |> Tuple.first) model.initial msgs
                        , messages = List.reverse msgs
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | input = toString err }, Cmd.none )


encodeMessages : List Msg -> String
encodeMessages messages =
    messages
        |> List.reverse
        |> List.map encodeMsg
        |> Encode.list
        |> Encode.encode 2


wrapperView :
    (model -> Html msg)
    -> WrappedModel model Msg
    -> Html (WrappedMsg msg)
wrapperView wrappedView model =
    Html.div []
        [ wrappedView model.wrapped |> Html.map Wrapped
        , Html.textarea
            [ Attr.style [ ( "width", "400px" ), ( "height", "200px" ) ]
            , Attr.value (encodeMessages model.messages)
            ]
            []
        , Html.textarea
            [ Attr.style [ ( "width", "400px" ), ( "height", "200px" ) ]
            , Attr.value model.input
            , Events.onInput Update
            ]
            []
        , Html.button [ Events.onClick Replay ] [ Html.text "replay" ]
        ]


type alias QAProgram flags model msg =
    Program flags (WrappedModel model msg) (WrappedMsg msg)


qaProgram :
    { a
        | init : ( model, Cmd Msg )
        , subscriptions : model -> Sub Msg
        , update : Msg -> model -> ( model, Cmd Msg )
        , view : model -> Html Msg
    }
    -> Program Never (WrappedModel model Msg) (WrappedMsg Msg)
qaProgram config =
    Html.program
        { init = wrapperInit config.init
        , update = wrapperUpdate config.update
        , view = wrapperView config.view
        , subscriptions =
            .wrapped >> config.subscriptions >> Sub.map Wrapped
        }
