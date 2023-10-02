module Game exposing (Game, Player, add, init, roll, wasCorrectlyAnswered, wrongAnswer)

import Array exposing (Array)
import Rope exposing (Rope)


type alias Game =
    { players : Array Player
    , popQuestions : List String
    , scienceQuestions : List String
    , sportsQuestions : List String
    , rockQuestions : List String
    , currentPlayer : Int
    , isGettingOutOfPenaltyBox : Bool
    }


type alias Player =
    { name : String
    , place : Int
    , purse : Int
    , inPenaltyBox : Bool
    }


init : ( Game, Rope String )
init =
    ( { players = Array.empty
      , popQuestions = createQuestions "Pop"
      , scienceQuestions = createQuestions "Science"
      , sportsQuestions = createQuestions "Sports"
      , rockQuestions = createQuestions "Rock"
      , currentPlayer = 0
      , isGettingOutOfPenaltyBox = False
      }
    , Rope.empty
    )


createQuestions : String -> List String
createQuestions category =
    List.range 0 49
        |> List.map (createQuestion category)


createQuestion : String -> Int -> String
createQuestion category i =
    category ++ " Question " ++ String.fromInt i


add : String -> Game -> ( Game, Rope String )
add playerName this =
    let
        newGame : Game
        newGame =
            { this
                | players =
                    Array.push
                        { name = playerName
                        , place = 0
                        , purse = 0
                        , inPenaltyBox = False
                        }
                        this.players
            }
    in
    ( newGame
    , Rope.fromList
        [ playerName ++ " was added"
        , "They are player number " ++ String.fromInt (Array.length newGame.players)
        ]
    )


roll : Int -> Game -> ( Game, Rope String )
roll roll_ this =
    let
        initialLogs : Rope String
        initialLogs =
            [ currentPlayer.name ++ " is the current player"
            , "They have rolled a " ++ String.fromInt roll_
            ]
                |> Rope.fromList

        ( next, logs ) =
            if currentPlayer.inPenaltyBox then
                if modBy 2 roll_ /= 0 then
                    let
                        next_ : Game
                        next_ =
                            { this
                                | isGettingOutOfPenaltyBox = True
                            }

                        nextPlayer : Player
                        nextPlayer =
                            { currentPlayer
                                | place = currentPlayer.place + roll_
                                , inPenaltyBox = False
                            }

                        nextPlayer_ : Player
                        nextPlayer_ =
                            if nextPlayer.place > 11 then
                                { nextPlayer | place = nextPlayer.place - 12 }

                            else
                                nextPlayer

                        next__ : Game
                        next__ =
                            { next_
                                | players = Array.set this.currentPlayer nextPlayer_ this.players
                            }

                        ( next___, askLogs ) =
                            askQuestion next__
                    in
                    ( next___
                    , [ currentPlayer.name ++ " is getting out of the penalty box"
                      , currentPlayer.name ++ "'s new location is " ++ String.fromInt nextPlayer_.place
                      , "The category is " ++ currentCategory next__
                      ]
                        |> Rope.fromList
                        |> Rope.prependTo askLogs
                    )

                else
                    ( { this
                        | isGettingOutOfPenaltyBox = False
                      }
                    , (currentPlayer.name ++ " is not getting out of the penalty box")
                        |> Rope.singleton
                    )

            else
                let
                    nextPlayer : Player
                    nextPlayer =
                        { currentPlayer
                            | place = currentPlayer.place + roll_
                            , inPenaltyBox = False
                        }

                    nextPlayer_ : Player
                    nextPlayer_ =
                        if nextPlayer.place > 11 then
                            { nextPlayer | place = nextPlayer.place - 12 }

                        else
                            nextPlayer

                    next__ : Game
                    next__ =
                        { this
                            | players = Array.set this.currentPlayer nextPlayer_ this.players
                        }

                    ( next___, askLogs ) =
                        askQuestion next__
                in
                ( next___
                , [ currentPlayer.name ++ "'s new location is " ++ String.fromInt nextPlayer_.place
                  , "The category is " ++ currentCategory next__
                  ]
                    |> Rope.fromList
                    |> Rope.prependTo askLogs
                )

        currentPlayer : Player
        currentPlayer =
            getUnsafe this.players this.currentPlayer
    in
    ( next, Rope.appendTo initialLogs logs )


askQuestion : Game -> ( Game, Rope String )
askQuestion game =
    let
        category : String
        category =
            currentCategory game

        pop : List String -> Rope String
        pop questions =
            questions
                |> List.head
                |> Maybe.withDefault ("--- out of " ++ category ++ " questions ---")
                |> Rope.singleton
    in
    case category of
        "Rock" ->
            ( { game | rockQuestions = List.drop 1 game.rockQuestions }
            , pop game.rockQuestions
            )

        "Sports" ->
            ( { game | sportsQuestions = List.drop 1 game.sportsQuestions }
            , pop game.sportsQuestions
            )

        "Science" ->
            ( { game | scienceQuestions = List.drop 1 game.scienceQuestions }
            , pop game.scienceQuestions
            )

        "Pop" ->
            ( { game | popQuestions = List.drop 1 game.popQuestions }
            , pop game.popQuestions
            )

        _ ->
            ( game, Rope.empty )


currentCategory : Game -> String
currentCategory this =
    let
        currentPlace : Int
        currentPlace =
            (getUnsafe this.players this.currentPlayer).place
    in
    case modBy 4 currentPlace of
        0 ->
            "Pop"

        1 ->
            "Science"

        2 ->
            "Sports"

        _ ->
            "Rock"


wasCorrectlyAnswered : Game -> ( Bool, Game, Rope String )
wasCorrectlyAnswered this =
    if (getUnsafe this.players this.currentPlayer).inPenaltyBox then
        if this.isGettingOutOfPenaltyBox then
            let
                currentPlayer : Player
                currentPlayer =
                    getUnsafe this.players this.currentPlayer

                nextPlayer : Player
                nextPlayer =
                    { currentPlayer | purse = currentPlayer.purse + 1 }

                next : Game
                next =
                    { this
                        | players = Array.set this.currentPlayer nextPlayer this.players
                    }

                winner : Bool
                winner =
                    didPlayerWin next

                next_ : Game
                next_ =
                    { next
                        | currentPlayer = next.currentPlayer + 1
                    }

                next__ : Game
                next__ =
                    if next_.currentPlayer == Array.length next_.players then
                        { next_ | currentPlayer = 0 }

                    else
                        next_
            in
            ( winner
            , next__
            , [ "Answer was corrent!!!!"
              , nextPlayer.name ++ " now has " ++ String.fromInt nextPlayer.purse ++ " Gold Coins."
              ]
                |> Rope.fromList
            )

        else
            let
                next : Game
                next =
                    { this
                        | currentPlayer = this.currentPlayer + 1
                    }

                next_ : Game
                next_ =
                    if next.currentPlayer == Array.length next.players then
                        { next | currentPlayer = 0 }

                    else
                        next
            in
            ( True
            , next_
            , Rope.empty
            )

    else
        let
            currentPlayer : Player
            currentPlayer =
                getUnsafe this.players this.currentPlayer

            nextPlayer : Player
            nextPlayer =
                { currentPlayer | purse = currentPlayer.purse + 1 }

            next : Game
            next =
                { this
                    | players = Array.set this.currentPlayer nextPlayer this.players
                }

            winner : Bool
            winner =
                didPlayerWin next

            next_ : Game
            next_ =
                { next
                    | currentPlayer = next.currentPlayer + 1
                }

            next__ : Game
            next__ =
                if next_.currentPlayer == Array.length next_.players then
                    { next_ | currentPlayer = 0 }

                else
                    next_
        in
        ( winner
        , next__
        , [ "Answer was corrent!!!!"
          , nextPlayer.name ++ " now has " ++ String.fromInt nextPlayer.purse ++ " Gold Coins."
          ]
            |> Rope.fromList
        )


wrongAnswer : Game -> ( Bool, Game, Rope String )
wrongAnswer this =
    let
        currentPlayer : Player
        currentPlayer =
            getUnsafe this.players this.currentPlayer

        nextPlayer : Player
        nextPlayer =
            { currentPlayer | inPenaltyBox = True }

        next : Game
        next =
            { this
                | currentPlayer = this.currentPlayer + 1
                , players = Array.set this.currentPlayer nextPlayer this.players
            }

        next_ : Game
        next_ =
            if next.currentPlayer == Array.length next.players then
                { next | currentPlayer = 0 }

            else
                next
    in
    ( True
    , next_
    , [ "Question was incorrectly answered"
      , currentPlayer.name ++ " was sent to the penalty box"
      ]
        |> Rope.fromList
    )


didPlayerWin : Game -> Bool
didPlayerWin this =
    (getUnsafe this.players this.currentPlayer).purse /= 6



-- Utilities to make the Array API more like imperative code
-- You _should_ clean these up


getUnsafe : Array a -> Int -> a
getUnsafe arr index =
    case Array.get index arr of
        Nothing ->
            Debug.todo <| "Out of boundary: " ++ String.fromInt index ++ " out of " ++ String.fromInt (Array.length arr)

        Just e ->
            e
