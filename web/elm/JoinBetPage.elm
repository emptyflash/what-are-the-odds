module JoinBetPage exposing (..)

import Constants
import Routing exposing (Route(JoinBet))
import Html exposing (..)
import Html.Attributes exposing (type_, placeholder)
import Html.Events exposing (onSubmit, onInput)
import Phoenix.Socket as Socket exposing (Socket)
import Phoenix.Channel as Channel
import Phoenix.Push as Push
import Json.Decode as Decoder exposing (Decoder)
import Json.Encode as Encoder
import Task
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input


type alias Model =
    { code : Maybe String
    , socket : Socket Msg
    , joinResponse : Maybe JoinResponse
    , odds : Int
    }


type Msg
    = PhoenixMsg (Socket.Msg Msg)
    | JoinChannel
    | SubmitOdds
    | UpdateCode String
    | UpdateOdds String
    | ChannelJoined Decoder.Value
    | ChannelError Decoder.Value
    | GoToGuessPage
    | ChangeRoute (Route Msg)


type alias JoinResponse =
    { bet : String
    , name : String
    }


decodeJoin : Decoder JoinResponse
decodeJoin =
    Decoder.map2 JoinResponse
        (Decoder.field "bet" Decoder.string)
        (Decoder.field "name" Decoder.string)


init : Route msg -> ( Model, Cmd Msg )
init route =
    let
        model =
            { socket = initSocket
            , code = Nothing
            , joinResponse = Nothing
            , odds = -1
            }

        modelAndCmd =
            model ! [ Cmd.none ]
    in
        case route of
            JoinBet code ->
                code
                    |> Maybe.map (joinChannel model)
                    |> Maybe.withDefault modelAndCmd

            _ ->
                modelAndCmd


initSocket : Socket Msg
initSocket =
    Socket.init Constants.websocketUrl
        |> Socket.withDebug


subscriptions : Model -> Sub Msg
subscriptions model =
    Socket.listen model.socket PhoenixMsg


channelName : String -> String
channelName code =
    "bet:join:" ++ code


joinChannel : Model -> String -> ( Model, Cmd Msg )
joinChannel model code =
    let
        channel =
            Channel.init (channelName code)
                |> Channel.onJoin ChannelJoined
                |> Channel.onError ChannelError

        ( newSocket, cmd ) =
            Socket.join channel model.socket
    in
        ( { model | socket = newSocket, code = Just code }
        , Cmd.map PhoenixMsg cmd
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PhoenixMsg msg ->
            let
                ( newSocket, cmd ) =
                    Socket.update msg model.socket
            in
                ( { model | socket = newSocket }, Cmd.map PhoenixMsg cmd )

        ChannelJoined json ->
            let
                join =
                    json
                        |> Decoder.decodeValue decodeJoin
                        |> Result.toMaybe
            in
                ( { model | joinResponse = join }, Cmd.none )

        UpdateCode code ->
            ( { model | code = Just code }, Cmd.none )

        UpdateOdds oddsString ->
            let
                odds =
                    oddsString
                        |> String.toInt
                        |> Result.toMaybe
                        |> Maybe.withDefault -1
            in
                ( { model | odds = odds }, Cmd.none )

        SubmitOdds ->
            let
                payload =
                    Encoder.object
                        [ ( "odds"
                          , Encoder.int model.odds
                          )
                        ]

                code =
                    Maybe.withDefault "" model.code

                channel =
                    channelName code

                push =
                    Push.init "new:odds" channel
                        |> Push.withPayload payload

                ( newSocket, cmd ) =
                    Socket.push push model.socket

                newestSocket =
                    Socket.on
                        "received:odds"
                        channel
                        (always GoToGuessPage)
                        newSocket
            in
                ( { model
                    | socket = newestSocket
                  }
                , Cmd.map PhoenixMsg cmd
                )

        GoToGuessPage ->
            Maybe.withDefault "" model.code
                |> channelName
                |> Routing.Guess model.odds model.socket
                |> Task.succeed
                |> Task.perform ChangeRoute
                |> (,) model

        JoinChannel ->
            case model.code of
                Just code ->
                    joinChannel model code

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.joinResponse of
        Just joinResponse ->
            Form.form [ onSubmit SubmitOdds ]
                [ Form.group []
                    [ Form.label []
                        [ text
                            (joinResponse.name
                                ++ " asked: "
                                ++ joinResponse.bet
                                ++ "?"
                            )
                        ]
                    , Input.number
                        [ Input.onInput UpdateOdds
                        , Input.attrs
                            [ placeholder "one in ..."
                            , Html.Attributes.min "2"
                            ]
                        ]
                    ]
                , Button.button
                    [ Button.attrs [ type_ "submit" ]
                    , Button.primary
                    ]
                    [ text "Submit" ]
                ]

        Nothing ->
            Form.form [ onSubmit JoinChannel ]
                [ Form.group []
                    [ Form.label
                        []
                        [ text "Please input the code from your friend:" ]
                    , Input.text [ Input.onInput UpdateCode ]
                    ]
                , Button.button
                    [ Button.attrs [ type_ "submit" ]
                    , Button.primary
                    ]
                    [ text "Submit" ]
                ]
