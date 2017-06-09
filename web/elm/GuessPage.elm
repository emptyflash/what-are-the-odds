module GuessPage exposing (..)

import StartBetPage
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput, onClick)
import Routing exposing (Route)
import Phoenix.Socket as Socket exposing (Socket)
import Json.Decode as Decoder
import Json.Encode as Encoder
import Phoenix.Push as Push


type Msg
    = PhoenixMsg (Socket.Msg Msg)
    | GuessReceived Decoder.Value
    | ChangeGuess String
    | SubmitGuess
    | NoOp


type alias Model =
    { odds : Int
    , guess : Maybe Int
    , otherGuess : Maybe Int
    , socket : Socket Msg
    , channelName : String
    , submitted : Bool
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Socket.listen model.socket PhoenixMsg


init : Route msg -> ( Model, Cmd Msg )
init route =
    case route of
        Routing.Guess odds socket channelName ->
            let
                push =
                    Push.init "received:odds" channelName

                ( newSocket, cmd ) =
                    socket
                        |> Socket.map (always NoOp)
                        |> Socket.on "new:guess" channelName GuessReceived
                        |> Socket.push push
            in
                { odds = odds
                , guess = Nothing
                , otherGuess = Nothing
                , socket = newSocket
                , channelName = channelName
                , submitted = False
                }
                    ! [ Cmd.map PhoenixMsg cmd ]

        _ ->
            Debug.crash "FUCK"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGuess guessString ->
            let
                guess =
                    guessString
                        |> String.toInt
                        |> Result.toMaybe
            in
                ( { model | guess = guess }, Cmd.none )

        GuessReceived json ->
            let
                guessResult =
                    Decoder.decodeValue
                        (Decoder.field "guess" Decoder.int)
                        json

                guess =
                    guessResult
                        |> Result.toMaybe
            in
                ( { model | otherGuess = guess }, Cmd.none )

        SubmitGuess ->
            let
                guess =
                    Maybe.withDefault -1 model.guess

                payload =
                    Encoder.object
                        [ ( "guess", Encoder.int guess ) ]

                push =
                    Push.init "new:guess" model.channelName
                        |> Push.withPayload payload

                ( newSocket, cmd ) =
                    Socket.push push model.socket
            in
                ( { model | socket = newSocket, submitted = True }, Cmd.map PhoenixMsg cmd )

        _ ->
            ( model, Cmd.none )


inputGuess : Model -> Html Msg
inputGuess model =
    div []
        [ text
            ("Input a number between 1 and "
                ++ toString model.odds
                ++ ":"
            )
        , input [ onInput ChangeGuess, type_ "number" ] []
        , button [ onClick SubmitGuess ] [ text "Submit" ]
        ]


view : Model -> Html Msg
view model =
    case ( model.submitted, model.otherGuess ) of
        ( False, Nothing ) ->
            inputGuess model

        ( False, Just _ ) ->
            inputGuess model

        ( True, Nothing ) ->
            text "waiting for friend to guess"

        ( True, Just otherGuess ) ->
            let
                guess =
                    Maybe.withDefault -1 model.guess
            in
                div []
                    [ text ("your guess: " ++ toString guess)
                    , text ("their guess: " ++ toString otherGuess)
                    ]
