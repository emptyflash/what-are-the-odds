module StartPage exposing (..)

import Routing exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Card as Card


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


howToPlayText : String
howToPlayText =
    """
"What Are the Odds?" is a game played by daring someone
by saying "what are the odds you'll ...". The person responds
with some number. Both players choose a number between 1 and
the odds, and count down, saying their chosen number. If both
numbers chosen are the same, the person who was dare has to perform
the dare.
"""


aboutApp : String
aboutApp =
    """
This app works in a similar way. To start, press "Start Bet",
and input your dare and name. You'll be given a code to give
to the person you're daring (you'll want to leave this screen
open). Tell your friend to click "Join Bet" and input
the code. They'll be presented with your dare and asked to
input the odds. Finally, you'll both be allowed to input a
guess. Once you've both guess your guesses will be revealed.
"""


view : Model -> Html Msg
view model =
    if model.howToPlay then
        Card.config [ Card.outlineInfo ]
            |> Card.headerH3 [] [ text "How To Play" ]
            |> Card.block []
                [ Card.text [] [ text howToPlayText ]
                , Card.titleH3 [] [ text "About This App" ]
                , Card.text [] [ text aboutApp ]
                , Card.custom <|
                    Button.button
                        [ Button.primary
                        , Button.onClick BackClick
                        ]
                        [ text "Back" ]
                ]
            |> Card.view
    else
        Grid.containerFluid []
            [ Grid.row [ Row.middleSm ]
                [ Grid.col [ Col.sm12, Col.md4 ]
                    [ Button.linkButton
                        [ Button.attrs [ href "#/start" ]
                        , Button.success
                        , Button.large
                        ]
                        [ text "Start Bet" ]
                    ]
                , Grid.col [ Col.sm12, Col.md4 ]
                    [ Button.linkButton
                        [ Button.attrs [ href "#/join" ]
                        , Button.primary
                        , Button.large
                        ]
                        [ text "Join Bet" ]
                    ]
                , Grid.col [ Col.sm12, Col.md4 ]
                    [ Button.button
                        [ Button.onClick HowToPlayClick
                        , Button.info
                        , Button.large
                        ]
                        [ text "How to play" ]
                    ]
                ]
            ]
