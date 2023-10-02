module Main exposing (main, run)

import Game exposing (Game)
import Html exposing (Html, div, p, text)
import Random exposing (Seed)
import Rope exposing (Rope)


main : Html msg
main =
    view run


run : Rope String
run =
    let
        ( game, initialLogs ) =
            Game.init "Chet" [ "Pat", "Sue" ]
    in
    go game initialLogs (Random.initialSeed 0)


go : Game -> Rope String -> Seed -> Rope String
go game queue seed =
    let
        ( upToFive, seed_ ) =
            Random.step (Random.int 1 5) seed

        ( upToEight, nextSeed ) =
            Random.step (Random.int 0 8) seed_

        ( game_, rollLogs ) =
            Game.roll (upToFive + 1) game

        ( notAWinner, nextGame, answerLogs ) =
            if upToEight == 7 then
                Game.wrongAnswer game_

            else
                Game.wasCorrectlyAnswered game_

        nextQueue : Rope String
        nextQueue =
            Rope.appendTo
                (Rope.appendTo queue rollLogs)
                answerLogs
    in
    if notAWinner then
        go nextGame nextQueue nextSeed

    else
        nextQueue


view : Rope String -> Html msg
view games =
    games
        |> Rope.toList
        |> List.map (\line -> p [] [ text line ])
        |> div []



-- This part is used to collect the logs, you can ignore it
