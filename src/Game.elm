module Game exposing (Game, add, init, roll, wasCorrectlyAnswered, wrongAnswer)

import Array exposing (Array)
import Rope exposing (Rope)


type alias Game =
    { players : Array String
    , places : Array Int
    , purses : Array Int
    , inPenaltyBox : Array Bool
    , popQuestions : List String
    , scienceQuestions : List String
    , sportsQuestions : List String
    , rockQuestions : List String
    , currentPlayer : Int
    , isGettingOutOfPenaltyBox : Bool
    }


init : ( Game, Rope String )
init =
    ( { players = Array.empty
      , places = Array.repeat 6 0
      , purses = Array.repeat 6 0
      , inPenaltyBox = Array.repeat 6 False
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
                | players = Array.push playerName this.players
                , places = Array.set (howManyPlayers this) 0 this.places
                , purses = Array.set (howManyPlayers this) 0 this.purses
                , inPenaltyBox = Array.set (howManyPlayers this) False this.inPenaltyBox
            }
    in
    ( newGame
    , Rope.fromList
        [ playerName ++ " was added"
        , "They are player number " ++ String.fromInt (Array.length newGame.players)
        ]
    )


howManyPlayers : Game -> Int
howManyPlayers game =
    Array.length game.players


roll : Int -> Game -> ( Game, Rope String )
roll roll_ this =
    let
        initialLogs : Rope String
        initialLogs =
            [ getUnsafe this.players this.currentPlayer ++ " is the current player"
            , "They have rolled a " ++ String.fromInt roll_
            ]
                |> Rope.fromList

        ( next, logs ) =
            if getUnsafe this.inPenaltyBox this.currentPlayer then
                if modBy 2 roll_ /= 0 then
                    let
                        next_ : Game
                        next_ =
                            { this
                                | isGettingOutOfPenaltyBox = True
                                , places = Array.set this.currentPlayer (getUnsafe this.places this.currentPlayer + roll_) this.places
                                , inPenaltyBox = Array.set this.currentPlayer False this.inPenaltyBox
                            }

                        next__ : Game
                        next__ =
                            if getUnsafe next_.places this.currentPlayer > 11 then
                                { next_ | places = Array.set this.currentPlayer (getUnsafe next_.places this.currentPlayer - 12) next_.places }

                            else
                                next_

                        ( next___, askLogs ) =
                            askQuestion next__
                    in
                    ( next___
                    , [ getUnsafe this.players this.currentPlayer ++ " is getting out of the penalty box"
                      , getUnsafe next__.players next__.currentPlayer ++ "'s new location is " ++ String.fromInt (getUnsafe next__.places next__.currentPlayer)
                      , "The category is " ++ currentCategory next__
                      ]
                        |> Rope.fromList
                        |> Rope.prependTo askLogs
                    )

                else
                    ( { this
                        | isGettingOutOfPenaltyBox = False
                      }
                    , (getUnsafe this.players this.currentPlayer ++ " is not getting out of the penalty box")
                        |> Rope.singleton
                    )

            else
                let
                    next_ : Game
                    next_ =
                        { this
                            | places = Array.set this.currentPlayer (getUnsafe this.places this.currentPlayer + roll_) this.places
                        }

                    next__ : Game
                    next__ =
                        if getUnsafe next_.places this.currentPlayer > 11 then
                            { next_ | places = Array.set this.currentPlayer (getUnsafe next_.places this.currentPlayer - 12) next_.places }

                        else
                            next_

                    ( next___, askLogs ) =
                        askQuestion next__
                in
                ( next___
                , [ getUnsafe next__.players next__.currentPlayer ++ "'s new location is " ++ String.fromInt (getUnsafe next__.places next__.currentPlayer)
                  , "The category is " ++ currentCategory next__
                  ]
                    |> Rope.fromList
                    |> Rope.prependTo askLogs
                )
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
            getUnsafe this.places this.currentPlayer
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
    if getUnsafe this.inPenaltyBox this.currentPlayer then
        if this.isGettingOutOfPenaltyBox then
            let
                next : Game
                next =
                    { this | purses = Array.set this.currentPlayer (getUnsafe this.purses this.currentPlayer + 1) this.purses }

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
              , getUnsafe next.players next.currentPlayer ++ " now has " ++ String.fromInt (getUnsafe next.purses next.currentPlayer) ++ " Gold Coins."
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
            next : Game
            next =
                { this | purses = Array.set this.currentPlayer (getUnsafe this.purses this.currentPlayer + 1) this.purses }

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
          , getUnsafe next.players next.currentPlayer ++ " now has " ++ String.fromInt (getUnsafe next.purses next.currentPlayer) ++ " Gold Coins."
          ]
            |> Rope.fromList
        )


wrongAnswer : Game -> ( Bool, Game, Rope String )
wrongAnswer this =
    let
        next : Game
        next =
            { this
                | inPenaltyBox = Array.set this.currentPlayer True this.inPenaltyBox
                , currentPlayer = this.currentPlayer + 1
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
      , getUnsafe this.players this.currentPlayer ++ " was sent to the penalty box"
      ]
        |> Rope.fromList
    )


didPlayerWin : Game -> Bool
didPlayerWin this =
    getUnsafe this.purses this.currentPlayer /= 6



-- Utilities to make the Array API more like imperative code
-- You _should_ clean these up


getUnsafe : Array a -> Int -> a
getUnsafe arr index =
    case Array.get index arr of
        Nothing ->
            Debug.todo <| "Out of boundary: " ++ String.fromInt index ++ " out of " ++ String.fromInt (Array.length arr)

        Just e ->
            e
