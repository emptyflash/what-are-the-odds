module GuessPage exposing (..)

import StartBetPage
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput, onSubmit)
import Routing exposing (Route)
import Phoenix.Socket as Socket exposing (Socket)
import Json.Decode as Decoder
import Json.Encode as Encoder
import Phoenix.Push as Push
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Badge as Badge
import Bootstrap.Form.Input as Input


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
    Form.form [ onSubmit SubmitGuess ]
        [ Form.group []
            [ Form.label []
                [ text
                    ("Input a number between 1 and "
                        ++ toString model.odds
                        ++ ":"
                    )
                ]
            , Input.number
                [ Input.onInput ChangeGuess
                , Input.defaultValue "1"
                , Input.attrs
                    [ Html.Attributes.max
                        (model.guess
                            |> Maybe.map toString
                            |> Maybe.withDefault ""
                        )
                    , Html.Attributes.min "1"
                    ]
                ]
            ]
        , Button.button
            [ Button.attrs [ type_ "submit" ]
            , Button.primary
            ]
            [ text "Submit" ]
        ]


view : Model -> Html Msg
view model =
    case ( model.submitted, model.otherGuess ) of
        ( False, Nothing ) ->
            inputGuess model

        ( False, Just _ ) ->
            inputGuess model

        ( True, Nothing ) ->
            p [] [ b [] [ text "waiting for friend to guess" ] ]

        ( True, Just otherGuess ) ->
            let
                guess =
                    Maybe.withDefault -1 model.guess

                theirBadge =
                    if guess == otherGuess then
                        Badge.badgeSuccess
                    else
                        Badge.badgeDanger
            in
                div []
                    [ h3 []
                        [ text "Your guess: "
                        , Badge.badgePrimary [] [ text <| toString guess ]
                        ]
                    , h3 []
                        [ text "Their guess: "
                        , theirBadge [] [ text <| toString otherGuess ]
                        ]
                    ]
