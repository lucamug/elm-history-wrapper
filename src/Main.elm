port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Dom.Scroll
import Task


port scrollOrResize : (String -> msg) -> Sub msg


type alias Model =
    { position : Float
    , counter : Int
    }


type Msg
    = Increment
    | Decrement
    | Reset
    | OnScroll String
    | ScrollTo Float
    | None


init : ( Model, Cmd msg )
init =
    ( { position = 0, counter = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        Reset ->
            ( { model | counter = 0 }, Cmd.none )

        OnScroll text ->
            ( model, attemptToGetScrollTop )

        ScrollTo position ->
            ( { model | position = position }, Cmd.none )

        None ->
            ( model, Cmd.none )


containerId1 : String
containerId1 =
    "ciao693"


attemptToGetScrollTop : Cmd Msg
attemptToGetScrollTop =
    Task.attempt getScrollTopResult (Dom.Scroll.y containerId1)


attemptToSetScrollTop2 : Float -> Cmd Msg
attemptToSetScrollTop2 position =
    let
        _ =
            Debug.log "attemptToSetScrollTop2" position
    in
        Task.attempt setScrollTopResult (Dom.Scroll.toY containerId1 position)


setScrollTopResult : a -> Msg
setScrollTopResult result =
    let
        _ =
            Debug.log "setScrollTopResult" result
    in
        None


getScrollTopResult : Result error Float -> Msg
getScrollTopResult result =
    case result of
        Ok position ->
            ScrollTo position

        Err error ->
            None


view : Model -> Html Msg
view counter =
    Html.div []
        [ Html.button [ onClick Decrement ] [ Html.text "-" ]
        , Html.text <| toString counter
        , Html.button [ onClick Increment ] [ Html.text "+" ]
        , Html.button [ onClick Reset ] [ Html.text "reset" ]
        , content "ciao693" "#ccccff"
        ]


content : String -> String -> Html msg
content containerId color =
    div
        [ id containerId
        , style
            [ ( "margin", "2em" )
            , ( "height", "200px" )
            , ( "overflow-y", "scroll" )
            , ( "background-color", color )
            ]
        ]
        [ div [ style [ ( "margin", "2em" ) ] ]
            [ h1 [] [ text "Simple Test" ]
            , div []
                (List.repeat 15
                    (div []
                        [ h2 [] [ text "Paragraph" ]
                        , p [] [ text """
    Nel mezzo del cammin di nostra vita
    mi ritrovai per una selva oscura,
    ché la diritta via era smarrita.
    Ahi quanto a dir qual era è cosa dura
    esta selva selvaggia e aspra e forte
    che nel pensier rinova la paura!
    Tant' è amara che poco è più morte;
    ma per trattar del ben ch'i' vi trovai,
    dirò de l'altre cose ch'i' v'ho scorte.
    Io non so ben ridir com' i' v'intrai,
    tant' era pien di sonno a quel punto
    che la verace via abbandonai.
    Ma poi ch'i' fui al piè d'un colle giunto,
    là dove terminava quella valle
    che m'avea di paura il cor compunto,
    guardai in alto e vidi le sue spalle
    vestite già de' raggi del pianeta
    che mena dritto altrui per ogne calle.
    Allor fu la paura un poco queta,
    che nel lago del cor m'era durata
    la notte ch'i' passai con tanta pieta.""" ]
                        ]
                    )
                )
            ]
        ]


encodeObject : String -> (a -> Value) -> a -> Value
encodeObject key ty value =
    Encode.object
        [ ( "key", Encode.string key )
        , ( "value", ty value )
        ]


encodeMsg : Msg -> Value
encodeMsg msg =
    case msg of
        Increment ->
            encodeObject "increment" Encode.int 1

        Decrement ->
            encodeObject "decrement" Encode.int 1

        Reset ->
            encodeObject "reset" Encode.int 0

        ScrollTo position ->
            encodeObject "scrollTo" Encode.float position

        _ ->
            encodeObject "none" Encode.float 0


type alias Element =
    { key : String
    , value : Int
    }


elementDecoder : Decoder Element
elementDecoder =
    Decode.map2 Element
        (Decode.field "key" Decode.string)
        (Decode.field "value" Decode.int)


decodeMsg : Decoder Msg
decodeMsg =
    -- Decode.list elementDecoder
    Decode.succeed Increment



{--
        |> Decode.andThen
            (\msgObj ->
                case msgObj of
                    "increment" ->
                        Decode.succeed Increment

                    "decrement" ->
                        Decode.succeed Decrement

                    "reset" ->
                        Decode.succeed Reset

                    _ ->
                        Decode.fail "unknown msg type"
            )
--}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ scrollOrResize OnScroll
        ]


main : QAProgram Never Model Msg
main =
    qaProgram
        { encode = encodeMsg
        , decode = decodeMsg
        }
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
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
      , input = ""
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
                        | input = ""
                        , wrapped = List.foldl (\msg model -> wrappedUpdate msg model |> Tuple.first) model.initial msgs
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
            , Html.textarea [ value model.input, onInput Update ] []
            , Html.br [] []
            , Html.button [ onClick Replay ] [ Html.text "replay" ]
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
