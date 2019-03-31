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
                    if this.GameBoard.ContainsKey(row, col) then
                        match this.GameBoard.Item(row,col) with
                        | Player.Cross -> "X"
                        | Player.Nought -> "O"
                    else
                        ""

        let CreateMove row col = 
            {
                Row = row;
                Column = col
             }

        let ApplyMove (oldState:GameState) (move: Move) = 
            let newBoard = oldState.GameBoard.Add((move.Column, move.Row), oldState.GameTurn)
            let newPlayer = 
                match oldState.GameTurn with
                | Player.Cross -> Player.Nought
                | Player.Nought -> Player.Cross
            {GameSize = oldState.GameSize; GameBoard = newBoard; GameTurn = newPlayer}


        let Lines (size:int) : seq<seq<int*int>> = 
            let Rows : seq<seq<int*int>> = 
                seq {for x in 0 .. size-1 do
                        yield seq{for y in 0 .. size-1 do yield (x,y)}
                }
            let Columns : seq<seq<int*int>> = 
                seq {for y in 0 .. size-1 do
                        yield seq{for x in 0 .. size-1 do yield (x,y)}
                }
            let Diagonals =
                let left = seq{yield seq {for x in 0 .. size-1 do yield (x,x)}}
                let right = seq{yield seq {for x in 0 .. size-1 do yield (x, size-1 - (x%size))}}
                Seq.append left right
            Seq.append (Seq.append Rows Columns) Diagonals
            
        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> =
            let playersAsLine = seq{for coords in line do if game.GameBoard.ContainsKey(coords) then yield game.GameBoard.Item(coords)}

            let seqFull = Seq.length playersAsLine = game.GameSize

            if (not(Seq.isEmpty playersAsLine)) then
                let crossExists = Seq.exists(fun x -> x = Cross) playersAsLine
                let noughtExists = Seq.exists(fun x -> x = Nought) playersAsLine
                if (crossExists && noughtExists) then
                    Draw
                else if (crossExists && not(noughtExists) && seqFull) then
                    Win(Cross, line)
                else if (noughtExists && not(crossExists) && seqFull) then
                    Win(Nought, line)
                else
                    Undecided
            else
                Undecided


        let GameOutcome (game:GameState) : TicTacToeOutcome<Player> =

            let allLines = Lines game.GameSize
            let finished = Seq.length allLines = game.GameSize

            let results = seq{
                for line in allLines do
                    yield CheckLine game line
            }

            match (Seq.exists (fun x -> match x with
                                         | Win(_,_) -> true
                                         |_ -> false) results) with
                   | true -> Seq.find (fun x -> match x with
                                                      | Win(_,_) -> true
                                                      |_ -> false) results
                   | false -> match finished with
                                    | true -> Draw
                                    |_ -> Undecided
 

        let GameStart (firstPlayer:Player) (size:int) = 
                   {GameTurn = firstPlayer; GameSize = size; GameBoard = Map.empty<int*int, Player>}

        let MiniMax (game: GameState) = raise (System.NotImplementedException("MiniMax"))

        let MiniMaxWithPruning game = raise (System.NotImplementedException("MiniMaxWithPruning"))

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
            override this.FindBestMove(game) = raise (System.NotImplementedException("FindBestMove"))

        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = raise (System.NotImplementedException("FindBestMove"))