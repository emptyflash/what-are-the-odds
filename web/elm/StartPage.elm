module StartPage exposing (..)

import Routing exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)


type Msg
    = HowToPlayClick
    | BackClick


type alias Model =
    { howToPlay : Bool
    }


init : Route msg -> ( Model, Cmd Msg )
init route =
    { howToPlay = False }
        ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        HowToPlayClick ->
            ( { model | howToPlay = True }, Cmd.none )

        BackClick ->
            ( { model | howToPlay = False }, Cmd.none )


view : Model -> Html Msg
view model =
    if model.howToPlay then
        div []
            [ text "description of how to play"
            , button [ onClick BackClick ] [ text "Back" ]
            ]
    else
        div []
            [ a [ href "#/start" ] [ text "Start Bet" ]
            , a [ href "#/join" ] [ text "Join Bet" ]
            , button
                [ onClick HowToPlayClick ]
                [ text "How to play" ]
            ]
