module Game exposing (Game, Player, init, roll, wasCorrectlyAnswered, wrongAnswer)

import Deque exposing (Deque)
import Rope exposing (Rope)


type alias Game =
    { currentPlayer : Player
    , playersQueue : Deque Player
    , popQuestions : List String
    , scienceQuestions : List String
    , sportsQuestions : List String
    , rockQuestions : List String
    , isGettingOutOfPenaltyBox : Bool
    }


type alias Player =
    { name : String
    , place : Int
    , purse : Int
    , inPenaltyBox : Bool
    }


{-| Initialize a game. Requires the name of the first player and of the other players.
-}
init : String -> List String -> ( Game, Rope String )
init firstPlayer otherPlayers =
    ( { currentPlayer = toPlayer firstPlayer
      , playersQueue = Deque.fromList <| List.map toPlayer otherPlayers
      , popQuestions = createQuestions "Pop"
      , scienceQuestions = createQuestions "Science"
      , sportsQuestions = createQuestions "Sports"
      , rockQuestions = createQuestions "Rock"
      , isGettingOutOfPenaltyBox = False
      }
    , (firstPlayer :: otherPlayers)
        |> List.indexedMap
            (\index playerName ->
                Rope.fromList
                    [ playerName ++ " was added"
                    , "They are player number " ++ String.fromInt (index + 1)
                    ]
            )
        |> Rope.fromList
        |> Rope.concat
    )


toPlayer : String -> Player
toPlayer playerName =
    { name = playerName
    , place = 0
    , purse = 0
    , inPenaltyBox = False
    }


createQuestions : String -> List String
createQuestions category =
    List.range 0 49
        |> List.map (createQuestion category)


createQuestion : String -> Int -> String
createQuestion category i =
    category ++ " Question " ++ String.fromInt i


roll : Int -> Game -> ( Game, Rope String )
roll roll_ this =
    let
        initialLogs : Rope String
        initialLogs =
            [ this.currentPlayer.name ++ " is the current player"
            , "They have rolled a " ++ String.fromInt roll_
            ]
                |> Rope.fromList
    in
    if this.currentPlayer.inPenaltyBox && modBy 2 roll_ == 0 then
        ( { this
            | isGettingOutOfPenaltyBox = False
          }
        , initialLogs
            |> Rope.append (this.currentPlayer.name ++ " is not getting out of the penalty box")
        )

    else
        let
            currentPlayer : Player
            currentPlayer =
                this.currentPlayer

            newPlace : Int
            newPlace =
                (currentPlayer.place + roll_)
                    |> modBy 12

            playerAfterMove : Player
            playerAfterMove =
                { currentPlayer
                    | place = newPlace
                    , inPenaltyBox = False
                }

            category : String
            category =
                categoryForPlayer playerAfterMove

            ( afterQuestion, askLogs ) =
                askQuestionInCategory category this

            commonMessages : List String
            commonMessages =
                [ currentPlayer.name
                    ++ "'s new location is "
                    ++ String.fromInt newPlace
                , "The category is " ++ category
                ]

            messages : List String
            messages =
                if currentPlayer.inPenaltyBox then
                    (currentPlayer.name ++ " is getting out of the penalty box")
                        :: commonMessages

                else
                    commonMessages
        in
        ( { afterQuestion
            | isGettingOutOfPenaltyBox = True
            , currentPlayer = playerAfterMove
          }
        , messages
            |> Rope.fromList
            |> Rope.prependTo askLogs
            |> Rope.appendTo initialLogs
        )


askQuestionInCategory : String -> Game -> ( Game, Rope String )
askQuestionInCategory category game =
    let
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


categoryForPlayer : Player -> String
categoryForPlayer currentPlayer =
    case modBy 4 currentPlayer.place of
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
    if not this.currentPlayer.inPenaltyBox || this.isGettingOutOfPenaltyBox then
        let
            currentPlayer : Player
            currentPlayer =
                this.currentPlayer

            nextPlayer : Player
            nextPlayer =
                { currentPlayer | purse = currentPlayer.purse + 1 }

            next : Game
            next =
                { this
                    | currentPlayer = nextPlayer
                }
        in
        ( not <| didPlayerWin next
        , rotatePlayers next
        , [ "Answer was corrent!!!!"
          , currentPlayer.name ++ " now has " ++ String.fromInt nextPlayer.purse ++ " Gold Coins."
          ]
            |> Rope.fromList
        )

    else
        ( True
        , rotatePlayers this
        , Rope.empty
        )


rotatePlayers : Game -> Game
rotatePlayers next =
    case Deque.popFront next.playersQueue of
        ( Nothing, _ ) ->
            next

        ( Just front, newQueue ) ->
            { next
                | currentPlayer = front
                , playersQueue = Deque.pushBack next.currentPlayer newQueue
            }


wrongAnswer : Game -> ( Bool, Game, Rope String )
wrongAnswer this =
    let
        currentPlayer : Player
        currentPlayer =
            this.currentPlayer

        nextPlayer : Player
        nextPlayer =
            { currentPlayer | inPenaltyBox = True }

        next : Game
        next =
            rotatePlayers { this | currentPlayer = nextPlayer }

        next_ : Game
        next_ =
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
    this.currentPlayer.purse == 6
