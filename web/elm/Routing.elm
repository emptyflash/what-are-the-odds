module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import Phoenix.Socket as Socket exposing (Socket, Msg)


type Route msg
    = StartPage
    | StartBet
    | JoinBet (Maybe String)
    | Guess Int (Socket msg) String
    | NotFound


matchers : Parser (Route msg -> a) a
matchers =
    oneOf
        [ map StartPage top
        , map StartBet (s "start")
        , map JoinBet (s "join" <?> stringParam "code")
        ]


mapRoute : (a -> b) -> Route a -> Route b
mapRoute f route =
    case route of
        Guess odds socket channel ->
            let
                newSocket =
                    Socket.map f socket
            in
                Guess odds newSocket channel

        StartPage ->
            StartPage

        StartBet ->
            StartBet

        JoinBet code ->
            JoinBet code

        NotFound ->
            NotFound



-- workaround for bug in url parser
-- https://github.com/evancz/url-parser/issues/2


fixLocationQuery : Location -> Location
fixLocationQuery location =
    let
        hash =
            String.split "?" location.hash
                |> List.head
                |> Maybe.withDefault ""

        search =
            String.split "?" location.hash
                |> List.drop 1
                |> String.join "?"
                |> String.append "?"
    in
        { location | hash = hash, search = search }


parseLocation : Location -> Route msg
parseLocation location =
    case (parseHash matchers <| fixLocationQuery location) of
        Just route ->
            route

        Nothing ->
            NotFound
