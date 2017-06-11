module StartBetPage exposing (..)

import Constants
import Task
import Routing exposing (Route)
import Json.Decode as Decoder
import Json.Encode as Encoder
import Html exposing (..)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onSubmit, onInput)
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel
import Phoenix.Push as Push
import Hashids
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea


type alias Model =
    { betText : String
    , name : String
    , submitted : Bool
    , socket : Socket Msg
    }


type Msg
    = ChangeBetText String
    | ChangeName String
    | SubmitBet
    | BetSubmitted
    | PhoenixMsg (Socket.Msg Msg)
    | OddsReceived Decoder.Value
    | ChangeRoute (Route Msg)


init : Route msg -> ( Model, Cmd Msg )
init _ =
    { betText = ""
    , name = ""
    , submitted = False
    , socket = initSocket
    }
        ! [ Cmd.none ]


initSocket : Socket Msg
initSocket =
    Socket.init Constants.websocketUrl
        |> Socket.withDebug


isVowel : Char -> Bool
isVowel char =
    [ 'a', 'e', 'i', 'o', 'u' ]
        |> List.member char


uniqueToken : String -> String
uniqueToken text =
    (text
        |> Hashids.hashidsSimple
        |> Hashids.encode
    )
        9001


payload : Model -> Encoder.Value
payload model =
    Encoder.object
        [ ( "bet", Encoder.string model.betText )
        , ( "name", Encoder.string model.name )
        ]


channelName : Model -> String
channelName model =
    "bet:join:" ++ uniqueToken model.betText


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PhoenixMsg msg ->
            let
                ( newSocket, cmd ) =
                    Socket.update msg model.socket
            in
                ( { model | socket = newSocket }, Cmd.map PhoenixMsg cmd )

        ChangeBetText text ->
            ( { model | betText = text }, Cmd.none )

        ChangeName name ->
            ( { model | name = name }, Cmd.none )

        SubmitBet ->
            let
                channel =
                    Channel.init (channelName model)
                        |> Channel.withPayload (payload model)
                        |> Channel.onJoin (always BetSubmitted)

                ( newSocket, cmd ) =
                    Socket.join channel model.socket
            in
                ( { model | socket = newSocket }
                , Cmd.map PhoenixMsg cmd
                )

        BetSubmitted ->
            let
                newSocket =
                    Socket.on
                        "new:odds"
                        (channelName model)
                        OddsReceived
                        model.socket
            in
                ( { model | submitted = True, socket = newSocket }, Cmd.none )

        OddsReceived json ->
            let
                oddsResult =
                    Decoder.decodeValue
                        (Decoder.field "odds" Decoder.int)
                        json

                odds =
                    oddsResult
                        |> Result.toMaybe
                        |> Maybe.withDefault 0

                channel =
                    channelName model

                route =
                    Routing.Guess odds model.socket channel

                changeRouteCmd =
                    route
                        |> Task.succeed
                        |> Task.perform ChangeRoute
            in
                model ! [ changeRouteCmd ]

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Socket.listen model.socket PhoenixMsg


view : Model -> Html Msg
view model =
    if not model.submitted then
        Form.form [ onSubmit SubmitBet ]
            [ Form.group []
                [ Form.label [] [ text "Your bet:" ]
                , Textarea.textarea
                    [ Textarea.onInput ChangeBetText
                    , Textarea.rows 4
                    , Textarea.defaultValue "what are the odds that "
                    ]
                ]
            , Form.group []
                [ Form.label [] [ text "Your name:" ]
                , Input.text
                    [ Input.onInput ChangeName
                    ]
                ]
            , Button.button
                [ Button.attrs [ type_ "submit" ]
                , Button.primary
                ]
                [ text "Submit" ]
            ]
    else
        Grid.containerFluid []
            [ Grid.row [ Row.centerSm ]
                [ Grid.col [ Col.middleSm ]
                    [ p []
                        [ text "Tell your friend to join with this code: "
                        , b [] [ model.betText |> uniqueToken |> text ]
                        ]
                    ]
                ]
            , Grid.row [ Row.centerSm ]
                [ Grid.col [ Col.middleSm ]
                    [ p []
                        [ text "Waiting for friend to join and choose odds" ]
                    ]
                ]
            ]
