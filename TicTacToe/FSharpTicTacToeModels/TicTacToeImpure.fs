namespace QUT

    module FSharpImpureTicTacToeModel =

        type Player = Empty | Nought | Cross

        type Move = 
            {
                Row: int;
                Col: int;
            }
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Col

        type GameState = 
            {
                mutable GameTurn: Player;
                GameSize: int;
                mutable GameBoard: Player array;
            } 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.GameTurn
                member this.Size with get()    = this.GameSize
                member this.getPiece(row, col) = 
                    match this.GameBoard.[col + (row * this.GameSize)] with
                    | Player.Cross -> "X"
                    | Player.Nought -> "O"
                    |_ -> ""

        let GameContainsMove game move =
            game.GameBoard.[move.Col + (move.Row * game.GameSize)] <> Empty

        let CreateMove row col   = 
            {Row = row; Col = col;}

        let ApplyMove game move = 
            let newBoard = Array.create (game.GameSize*game.GameSize) Empty
            game.GameBoard.CopyTo(newBoard,0)
            newBoard.SetValue(game.GameTurn, move.Col + (move.Row * (game.GameSize)))

            {GameBoard = newBoard; GameSize = game.GameSize; GameTurn = match game.GameTurn with
                                                                        | Cross -> Nought
                                                                        | Nought -> Cross
                                                                        |_ -> game.GameTurn}                                                         

        let GameStart first size = 
            {
                GameSize = size;
                GameTurn = first;
                GameBoard = Array.create (size*size) Empty;
            }


        let Lines size = 
            seq{for x in 0 .. size-1 do yield seq{for y in 0 .. size-1 do yield (x,y)}}                  // Rows
            |> Seq.append (seq{for y in 0 .. size-1 do yield seq{for x in 0 .. size-1 do yield (x,y)}})  // Columns
            |> Seq.append (seq{yield seq {for x in 0 .. size-1 do yield (x,x)}})                         // Left diagonal
            |> Seq.append (seq{yield seq {for x in 0 .. size-1 do yield (x, size-1 - (x%size))}})        // Right diagonal     

        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> =
            let countPlayer player = Seq.fold(fun n elem -> if elem = player then n+1 else n) 0          // Count quantity of player type
                                            (seq{for coords in line do                                   // Line as a sequence of players
                                                    if (GameContainsMove game (CreateMove (fst(coords)) (snd(coords)) ) ) then 
                                                        yield game.GameBoard.[snd(coords) + (fst(coords) * game.GameSize)]}) 

            if (countPlayer(Nought) > 0 && countPlayer(Cross) > 0) then
                Draw
            else if (countPlayer(Nought) = game.GameSize) then
                Win(Nought, line)
            else if (countPlayer(Cross) = game.GameSize) then
                Win(Cross, line)
            else
                Undecided

        let GameOutcome game = 
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

        let TicTacToeHeuristic game player =
            match GameOutcome game with
            | Win(x,_) -> if (x=player) then 1 else -1
            |_ -> 0

        let TicTacToeGetTurn game =
            game.GameTurn

        let TicTacToeGameOver game = 
            match GameOutcome game with
            | Undecided -> false
            |_ -> true

        let TicTacToeApplyMove game move =
            ApplyMove game (CreateMove (fst(move)) (snd(move)))
            
        let TicTacToeMoveGenerator game =
            let mutable moves = List.empty<(int*int)>
            for x in 0 .. game.GameSize-1 do
                for y in 0 .. game.GameSize-1 do
                    let move = CreateMove x y
                    if not (GameContainsMove game move) then
                        moves <- moves @ [(x,y)]
            List.toSeq moves

        let FindBestMove game = 
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
            |> (fun x -> fst(x).Value)
            |> (fun x -> CreateMove (fst(x)) (snd(x)))

        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game move
                member this.FindBestMove(game)           = FindBestMove game