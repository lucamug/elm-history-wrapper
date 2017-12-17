module Main exposing (main)

import Html exposing (Html, text)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Task
import Time


type alias Model =
    { counter : Int
    , error : String
    }


type Msg
    = Increment
    | Decrement
    | Reset
    | OnTime Time.Time
    | None


init : ( Model, Cmd msg )
init =
    { counter = 0, error = "" } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        Increment ->
            ( { model
                | counter = model.counter + 1
                , error = "OnTime not called yet"
              }
            , Task.perform OnTime Time.now
            )

        Decrement ->
            ( { model
                | counter = model.counter - 1
                , error = "OnTime not called yet"
              }
            , Task.perform OnTime Time.now
            )

        Reset ->
            ( { model
                | counter = 0
                , error = "OnTime not called yet"
              }
            , Task.perform OnTime Time.now
            )

        OnTime time ->
            -- OnTime called, cancelling "OnTime not called yet"
            ( { model | error = "" }
            , Cmd.none
            )

        None ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Events.onClick Decrement ] [ Html.text "-" ]
        , Html.text <| toString model.counter
        , Html.button [ Events.onClick Increment ] [ Html.text "+" ]
        , Html.button [ Events.onClick Reset ] [ Html.text "reset" ]
        , Html.div [ Attr.style [ ( "color", "red" ) ] ] [ text model.error ]
        ]


encodeMsg : Msg -> Value
encodeMsg msg =
    case msg of
        Increment ->
            Encode.string "increment"

        Decrement ->
            Encode.string "decrement"

        Reset ->
            Encode.string "reset"

        OnTime time ->
            Encode.string "onTime"

        None ->
            Encode.string "none"


decodeMsg : Decoder Msg
decodeMsg =
    Decode.string
        |> Decode.andThen
            (\msgString ->
                case msgString of
                    "increment" ->
                        Decode.succeed Increment

                    "decrement" ->
                        Decode.succeed Decrement

                    "reset" ->
                        Decode.succeed Reset

                    "onTime" ->
                        Decode.succeed None

                    "none" ->
                        Decode.succeed None

                    _ ->
                        Decode.fail "unknown msg type"
            )


main : QAProgram Never Model Msg
main =
    qaProgram
        { encode = encodeMsg
        , decode = decodeMsg
        }
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



---


type alias Codec msg =
    { encode : msg -> Encode.Value
    , decode : Decoder msg
    }


type alias WrappedModel model msg =
    { wrapped : model
    , initial : model
    , messages : List msg
    , codec : Codec msg
    , input : String
    }


wrapperInit :
    Codec msg
    -> ( model, Cmd msg )
    -> ( WrappedModel model msg, Cmd (WrappedMsg msg) )
wrapperInit codec ( model, msgs ) =
    ( { wrapped = model
      , initial = model
      , messages = []
      , codec = codec
      , input = """[
  "increment"
]"""
      }
    , Cmd.map Wrapped msgs
    )


type WrappedMsg msg
    = Wrapped msg
    | Update String
    | Replay


wrapperUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> WrappedMsg msg
    -> WrappedModel model msg
    -> ( WrappedModel model msg, Cmd (WrappedMsg msg) )
wrapperUpdate wrappedUpdate msg model =
    case msg of
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
            case Decode.decodeString (Decode.list model.codec.decode) model.input of
                Ok msgs ->
                    ( { model
                        | wrapped = List.foldl (\msg model -> wrappedUpdate msg model |> Tuple.first) model.initial msgs
                        , messages = List.reverse msgs
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | input = toString err }, Cmd.none )


wrapperView :
    (model -> Html msg)
    -> Codec msg
    -> WrappedModel model msg
    -> Html (WrappedMsg msg)
wrapperView wrappedView codec model =
    let
        messages : String
        messages =
            model.messages
                |> List.reverse
                |> List.map codec.encode
                |> Encode.list
                |> Encode.encode 2
    in
    Html.div []
        [ wrappedView model.wrapped |> Html.map Wrapped
        , Html.hr [] []
        , Html.pre [] [ Html.text messages ]
        , Html.textarea
            [ Attr.style [ ( "width", "400px" ), ( "height", "200px" ) ]
            , Attr.value model.input
            , Events.onInput Update
            ]
            []
        , Html.br [] []
        , Html.button [ Events.onClick Replay ] [ Html.text "replay" ]
        ]


type alias QAProgram flags model msg =
    Program flags (WrappedModel model msg) (WrappedMsg msg)


qaProgram :
    Codec msg
    ->
        { init : ( model, Cmd msg )
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Html msg
        , subscriptions : model -> Sub msg
        }
    -> QAProgram Never model msg
qaProgram codec config =
    Html.program
        { init = wrapperInit codec config.init
        , update = wrapperUpdate config.update
        , view = wrapperView config.view codec
        , subscriptions =
            .wrapped >> config.subscriptions >> Sub.map Wrapped
        }
