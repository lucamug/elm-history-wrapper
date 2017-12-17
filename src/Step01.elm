port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time


-- import Dom.Scroll

import Task


port scrollTo2321 : Float -> Cmd msg


type alias Model =
    { position : Float
    , counter : Int
    , error : String
    }


type Msg
    = Increment
    | Decrement
    | Reset
    | OnScroll Float
    | ScrollTo Float
    | ScrollTopResult String
    | None String
    | OnTime Time.Time


init : ( Model, Cmd msg )
init =
    ( { position = 0, counter = 0, error = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        Reset ->
            ( { model | counter = 0 }, Cmd.none )

        OnScroll position ->
            ( { model | position = position, error = "" }, Cmd.none )

        ScrollTo position ->
            let
                _ =
                    Debug.log "Here it should call OnTime..." ""
            in
                ( { model | error = "Here it should call Time.now" }
                , Cmd.batch
                    --[ Task.attempt setScrollTopResult (Dom.Scroll.toY containerId position)
                    -- [ scrollTo2321 position
                    [ Task.perform OnTime Time.now
                    ]
                )

        OnTime time ->
            let
                _ =
                    Debug.log "...OnTime called"
            in
                ( { model | error = model.error ++ "...OnTime called" }, Cmd.none )

        ScrollTopResult result ->
            ( model, Cmd.none )

        None text ->
            ( model, Cmd.none )


containerId : String
containerId =
    "ciao693"


setScrollTopResult : Result x a -> Msg
setScrollTopResult result =
    ScrollTopResult (toString result)


sectionStyle : Attribute Msg
sectionStyle =
    style
        [ ( "border", "1px solid #eee" )
        , ( "padding", "10px" )
        , ( "margin", "10px" )
        ]


buttonStyle : Attribute msg
buttonStyle =
    style
        [ ( "background-color", "#fee" )
        , ( "border-radius", "20px" )
        , ( "border", "2px solid #fdd" )
        , ( "padding", "10px" )
        , ( "margin", "5px" )
        , ( "font-size", "1.1em" )
        ]


view : Model -> Html Msg
view model =
    div []
        [ div
            [ sectionStyle ]
            [ button [ buttonStyle, onClick Decrement ] [ text " - " ]
            , div
                [ style
                    [ ( "display", "inline-block" )
                    , ( "padding", "0 10px" )
                    , ( "margin", "10px" )
                    , ( "font-size", "2em" )
                    , ( "border", "1px solid #eee" )
                    ]
                ]
                [ text <| toString model.counter ]
            , button [ buttonStyle, onClick Increment ] [ text " + " ]
            , button [ buttonStyle, onClick Reset ] [ text "reset" ]
            ]
        , div
            [ sectionStyle ]
            [ button [ buttonStyle, onClick (ScrollTo 0) ] [ text "Go to 0" ]
            , button [ buttonStyle, onClick (ScrollTo 50) ] [ text "Go to 50" ]
            , button [ buttonStyle, onClick (ScrollTo 100) ] [ text "Go to 100" ]
            , content "#ccccff"
            ]
        , div [ style [ ( "color", "red" ) ] ] [ text model.error ]
        ]


decodeOnScrollEvent : Decoder Float
decodeOnScrollEvent =
    Decode.at [ "target", "scrollTop" ] Decode.float


content : String -> Html Msg
content color =
    div
        [ id containerId
        , on "scroll" (Decode.map OnScroll decodeOnScrollEvent)
        , style
            [ ( "margin-top", "10px" )
            , ( "height", "100px" )
            , ( "overflow-y", "scroll" )
            , ( "background-color", color )
            ]
        ]
        [ div [ style [ ( "margin", "2em" ) ] ]
            [ div []
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


encodeMsg : Msg -> Value
encodeMsg msg =
    case msg of
        Increment ->
            Encode.object
                [ ( "msg", Encode.string "Increment" ) ]

        Decrement ->
            Encode.object
                [ ( "msg", Encode.string "Decrement" ) ]

        Reset ->
            Encode.object
                [ ( "msg", Encode.string "Reset" ) ]

        OnScroll position ->
            Encode.object
                [ ( "msg", Encode.string "OnScroll" )
                , ( "position", Encode.float position )
                ]

        ScrollTo position ->
            Encode.object
                [ ( "msg", Encode.string "ScrollTo" )
                , ( "position", Encode.float position )
                ]

        ScrollTopResult result ->
            Encode.object
                [ ( "msg", Encode.string "ScrollTopResult" )
                , ( "result", Encode.string result )
                ]

        OnTime time ->
            Encode.object
                [ ( "msg", Encode.string "OnTime" )
                , ( "position", Encode.float time )
                ]

        None text ->
            Encode.object
                [ ( "msg", Encode.string "None" )
                , ( "text", Encode.string text )
                ]


type alias Element =
    { key : String
    , value : Int
    }


elementDecoder : Decoder Element
elementDecoder =
    Decode.map2 Element
        (Decode.field "key" Decode.string)
        (Decode.field "value" Decode.int)



-- https://www.brianthicks.com/post/2016/06/17/how-does-json-decode-andthen-work/
-- http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode#andThen


decodeMsg : Decoder Msg
decodeMsg =
    Decode.field "msg" Decode.string
        |> Decode.andThen infoHelp


infoHelp : String -> Decoder Msg
infoHelp msg =
    case msg of
        "Increment" ->
            Decode.succeed Increment

        "Decrement" ->
            Decode.succeed Decrement

        "Reset" ->
            Decode.succeed Reset

        "ScrollTo" ->
            Decode.succeed (None "Ignoring ScrollTo during the playback...")

        "ScrollTopResult" ->
            Decode.succeed (None "Ignoring ScrollTopResult during the playback...")

        "OnScroll" ->
            Decode.field "position" Decode.float
                |> Decode.andThen (\position -> Decode.succeed (ScrollTo position))

        "OnTime" ->
            Decode.succeed (None "Ignoring OnTime...")

        "None" ->
            Decode.field "text" Decode.string
                |> Decode.andThen (\text -> Decode.succeed (None text))

        _ ->
            Decode.fail ("Unknown msg type: " ++ msg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []


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
    , error : String
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
  {
    "msg": "OnScroll",
    "position": 50
  }
]"""
      , error = ""
      }
    , Cmd.map Wrapped msgs
    )


type WrappedMsg msg
    = Wrapped msg
    | Update String
    | CopyHistory
    | Replay


wrapperUpdate :
    (Msg -> a -> ( a, Cmd msg ))
    -> WrappedMsg Msg
    ->
        { c
            | codec : { b | decode : Decoder Msg }
            , initial : a
            , input : String
            , messages : List Msg
            , wrapped : a
            , error : String
        }
    ->
        ( { c
            | codec : { b | decode : Decoder Msg }
            , error : String
            , initial : a
            , input : String
            , messages : List Msg
            , wrapped : a
          }
        , Cmd (WrappedMsg msg)
        )
wrapperUpdate wrappedUpdate msg model =
    case msg of
        CopyHistory ->
            -- ( { model | error = "Not implemented yet, please copy it manually" }, Cmd.none )
            ( { model | input = (encodeMessages model.messages) }, Cmd.none )

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
                        | error = ""
                        , wrapped = List.foldl (\msg model -> wrappedUpdate msg model |> Tuple.first) model.initial msgs
                        , messages = List.reverse msgs
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | error = toString err }, Cmd.none )



-- encodeMsg : Msg -> Value
-- encodeMessages : List a -> (a -> Value) -> String


encodeMessages : List Msg -> String
encodeMessages messages =
    messages
        |> List.reverse
        |> List.map encodeMsg
        |> Encode.list
        |> Encode.encode 2


wrapperView :
    (a -> Html msg)
    -> { c | encode : b }
    -> { d | error : String, input : String, messages : List Msg, wrapped : a }
    -> Html (WrappedMsg msg)
wrapperView wrappedView codec model =
    div []
        [ wrappedView model.wrapped |> Html.map Wrapped
        , div
            [ style
                [ ( "background-color", "pink" )
                , ( "padding", "20px" )
                ]
            ]
            [ h1 [] [ text "History Wrapper" ]
            , div []
                [ h2 [] [ text "History" ]
                , textarea [ value (encodeMessages model.messages) ]
                    []
                , button [ buttonStyle, onClick CopyHistory ] [ text "Copy History into Playback" ]
                ]
            , div
                []
                [ h2 [] [ text "Playback" ]
                , textarea
                    [ style
                        [ ( "width", "50%" )
                        , ( "height", "100px" )
                        ]
                    , value model.input
                    , onInput Update
                    ]
                    []
                , button [ buttonStyle, onClick Replay ] [ text "Replay" ]
                , div [ style [ ( "color", "red" ) ] ] [ text model.error ]
                ]
            ]
        ]


type alias QAProgram flags model msg =
    Program flags (WrappedModel model msg) (WrappedMsg msg)


qaProgram :
    Codec Msg
    ->
        { b
            | init : ( a, Cmd Msg )
            , subscriptions : a -> Sub Msg
            , update : Msg -> a -> ( a, Cmd Msg )
            , view : a -> Html Msg
        }
    -> Program Never (WrappedModel a Msg) (WrappedMsg Msg)
qaProgram codec config =
    Html.program
        { init = wrapperInit codec config.init
        , update = wrapperUpdate config.update
        , view = wrapperView config.view codec
        , subscriptions =
            .wrapped >> config.subscriptions >> Sub.map Wrapped
        }
