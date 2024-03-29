 namespace QUT

    module FSharpPureTicTacToeModel =

        type Player = Nought | Cross

        type Move = 
            {
                Row: int;
                Column: int
            }
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Column

        type GameState = 
            {
                GameTurn: Player;
                GameSize: int;
                GameBoard: Map<int*int, Player>
            }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.GameTurn
                member this.Size with get()    = this.GameSize
                member this.getPiece(row, col) = 
                    match this.GameBoard.TryFind(row, col) with
                    | Some player -> match player with 
                                     | Player.Cross -> "X"
                                     | Player.Nought -> "O"
                    |_ -> ""

        let CreateMove row col = 
            {
                Row = row;
                Column = col
            }

        let ApplyMove oldState move = 
            let newBoard = oldState.GameBoard.Add((move.Row, move.Column), oldState.GameTurn)
            let newPlayer = 
                match oldState.GameTurn with
                | Player.Cross -> Player.Nought
                | Player.Nought -> Player.Cross
            {GameSize = oldState.GameSize; GameBoard = newBoard; GameTurn = newPlayer}

        let Lines size = 
            seq{for x in 0 .. size-1 do yield seq{for y in 0 .. size-1 do yield (x,y)}}                  // Rows
            |> Seq.append (seq{for y in 0 .. size-1 do yield seq{for x in 0 .. size-1 do yield (x,y)}})  // Columns
            |> Seq.append (seq{yield seq {for x in 0 .. size-1 do yield (x,x)}})                         // Left diagonal
            |> Seq.append (seq{yield seq {for x in 0 .. size-1 do yield (x, size-1 - (x%size))}})        // Right diagonal     

        let CheckLine game line : TicTacToeOutcome<Player> =
            let countPlayer player = Seq.fold(fun n elem -> if elem = player then n+1 else n) 0          // Count quantity of player type
                                            (seq{for coords in line do                                   // Line as a sequence of players
                                                    if (game.GameBoard.ContainsKey(coords)) then 
                                                        yield game.GameBoard.Item(coords)}) 

            if (countPlayer(Nought) > 0 && countPlayer(Cross) > 0) then
                Draw
            else if (countPlayer(Nought) = game.GameSize) then
                Win(Nought, line)
            else if (countPlayer(Cross) = game.GameSize) then
                Win(Cross, line)
            else
                Undecided

        let GameOutcome game : TicTacToeOutcome<Player> =
            let gameOutcomes = Seq.map(fun line -> CheckLine game line) (Lines(game.GameSize))

            // If all line results are draw, then its a draw.
            if (Seq.forall(fun elem -> elem = Draw) gameOutcomes) then                          
                Draw                                                          
            // If a mixture of draw and undecided, then its undecided.       
            elif (Seq.forall(fun elem -> elem = Draw || elem = Undecided) gameOutcomes) then    
                Undecided  
            // Else that means there must be a win somewhere.                                                           
            else
                Seq.find(fun elem -> elem <> Draw && elem <> Undecided) gameOutcomes       

        let GameStart firstPlayer size = 
            {GameTurn = firstPlayer; GameSize = size; GameBoard = Map.empty<int*int, Player>}

        let TicTacToeHeuristic game player =
            match GameOutcome game with
            | Win(x,_) -> if (x=player) then 1 else -1
            |_ -> 0

        let TicTacToeMoveGenerator game =
            Seq.filter(fun x -> not(game.GameBoard.ContainsKey(x)))  // Create a sequence of moves not in map
                (seq{for x in 0 .. game.GameSize-1 do 
                      for y in 0 .. game.GameSize-1 do yield (x,y)}) // Given all the moves possible

        let TicTacToeGetTurn game =
            game.GameTurn

        let TicTacToeGameOver game = 
            match GameOutcome game with
            | Undecided -> false
            |_ -> true

        let TicTacToeApplyMove game move =
            ApplyMove game (CreateMove (fst(move)) (snd(move)))

        let MiniMax (game:GameState) = 
            GameTheory.MiniMaxGenerator 
                 TicTacToeHeuristic
                 TicTacToeGetTurn
                 TicTacToeGameOver
                 TicTacToeMoveGenerator
                 TicTacToeApplyMove
                 game
                 game.GameTurn

        let MiniMaxWithPruning game = 
            GameTheory.MiniMaxWithAlphaBetaPruningGenerator
                TicTacToeHeuristic
                TicTacToeGetTurn
                TicTacToeGameOver
                TicTacToeMoveGenerator
                TicTacToeApplyMove
                -1
                1
                game
                game.GameTurn

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) =
                fst(MiniMax game).Value
                |> (fun x -> CreateMove (fst(x)) (snd(x)))
                

        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = 
                fst(MiniMaxWithPruning game).Value
                |> (fun x -> CreateMove (fst(x)) (snd(x)))