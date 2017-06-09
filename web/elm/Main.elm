module Main exposing (..)

import StartPage
import StartBetPage
import JoinBetPage
import GuessPage
import Routing exposing (parseLocation, Route(..))
import Navigation exposing (Location)
import Html exposing (Html, text, h1)
import Json.Encode as Encoder
import Json.Decode as Decoder exposing (Decoder, field)
import Task
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = LocationChange Location
    | PageMsg PageMsg


type PageMsg
    = StartMsg StartPage.Msg
    | StartBetMsg StartBetPage.Msg
    | JoinBetMsg JoinBetPage.Msg
    | GuessMsg GuessPage.Msg


type PageModel
    = StartModel StartPage.Model
    | StartBetModel StartBetPage.Model
    | JoinBetModel JoinBetPage.Model
    | GuessModel GuessPage.Model
    | Empty


type alias Model =
    { route : Route PageMsg
    , pageModel : PageModel
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pageModel of
        StartBetModel startBetModel ->
            StartBetPage.subscriptions startBetModel
                |> Sub.map (PageMsg << StartBetMsg)

        JoinBetModel joinBetModel ->
            JoinBetPage.subscriptions joinBetModel
                |> Sub.map (PageMsg << JoinBetMsg)

        GuessModel guessModel ->
            GuessPage.subscriptions guessModel
                |> Sub.map (PageMsg << GuessMsg)

        _ ->
            Sub.none


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            parseLocation location

        model =
            { route = route
            , pageModel = Empty
            }
    in
        updateRoute model route


defaultInit : Route msg -> ( PageModel, Cmd PageMsg )
defaultInit route =
    route
        |> StartPage.init
        |> Tuple.mapFirst StartModel
        |> Tuple.mapSecond (Cmd.map StartMsg)


updateRoute : Model -> Route PageMsg -> ( Model, Cmd Msg )
updateRoute model route =
    let
        ( pageModel, pageCmd ) =
            case route of
                StartBet ->
                    route
                        |> StartBetPage.init
                        |> Tuple.mapFirst StartBetModel
                        |> Tuple.mapSecond (Cmd.map StartBetMsg)

                JoinBet _ ->
                    route
                        |> JoinBetPage.init
                        |> Tuple.mapFirst JoinBetModel
                        |> Tuple.mapSecond (Cmd.map JoinBetMsg)

                Guess _ _ _ ->
                    route
                        |> GuessPage.init
                        |> Tuple.mapFirst GuessModel
                        |> Tuple.mapSecond (Cmd.map GuessMsg)

                _ ->
                    defaultInit route
    in
        { model
            | route = route
            , pageModel = pageModel
        }
            ! [ Cmd.map PageMsg pageCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            location
                |> parseLocation
                |> updateRoute model

        PageMsg pageMsg ->
            case pageMsg of
                StartBetMsg (StartBetPage.ChangeRoute route) ->
                    route
                        |> Routing.mapRoute StartBetMsg
                        |> updateRoute model

                JoinBetMsg (JoinBetPage.ChangeRoute route) ->
                    route
                        |> Routing.mapRoute JoinBetMsg
                        |> updateRoute model

                _ ->
                    updatePage pageMsg model
                        |> Tuple.mapSecond (Cmd.map PageMsg)


updatePage : PageMsg -> Model -> ( Model, Cmd PageMsg )
updatePage pageMsg model =
    case ( pageMsg, model.pageModel ) of
        ( StartMsg msg, StartModel pageModel ) ->
            let
                ( newModel, cmd ) =
                    StartPage.update msg pageModel
            in
                ( { model | pageModel = StartModel newModel }
                , Cmd.map StartMsg cmd
                )

        ( StartBetMsg msg, StartBetModel pageModel ) ->
            let
                ( newModel, cmd ) =
                    StartBetPage.update msg pageModel
            in
                ( { model
                    | pageModel = StartBetModel newModel
                  }
                , Cmd.map StartBetMsg cmd
                )

        ( JoinBetMsg msg, JoinBetModel pageModel ) ->
            let
                ( newModel, cmd ) =
                    JoinBetPage.update msg pageModel
            in
                ( { model
                    | pageModel = JoinBetModel newModel
                  }
                , Cmd.map JoinBetMsg cmd
                )

        ( GuessMsg msg, GuessModel pageModel ) ->
            let
                ( newModel, cmd ) =
                    GuessPage.update msg pageModel
            in
                ( { model
                    | pageModel = GuessModel newModel
                  }
                , Cmd.map GuessMsg cmd
                )

        msgAndModel ->
            ( model, Cmd.none )
                |> Debug.log ("unexpected combo: " ++ toString msgAndModel)


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , h1 [] [ text "What Are the Odds?" ]
        , viewPage model
        ]


viewPage : Model -> Html Msg
viewPage model =
    case ( model.route, model.pageModel ) of
        ( StartPage, StartModel pageModel ) ->
            StartPage.view pageModel
                |> Html.map (PageMsg << StartMsg)

        ( StartBet, StartBetModel pageModel ) ->
            StartBetPage.view pageModel
                |> Html.map (PageMsg << StartBetMsg)

        ( JoinBet _, JoinBetModel pageModel ) ->
            JoinBetPage.view pageModel
                |> Html.map (PageMsg << JoinBetMsg)

        ( Guess _ _ _, GuessModel pageModel ) ->
            GuessPage.view pageModel
                |> Html.map (PageMsg << GuessMsg)

        a ->
            let
                a_ =
                    Debug.log "test" a
            in
                text "not found"
