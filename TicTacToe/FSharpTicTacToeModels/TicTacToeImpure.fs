namespace QUT

    module FSharpImpureTicTacToeModel =

        type Player = Nothing | Nought | Cross

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

        let CreateMove row col   = 
            {Row = row; Col = col;}

        let ApplyMove game move = 
            game.GameBoard.SetValue(move, move.Col + (move.Row * (game.GameSize)))

            game.GameTurn <- match game.GameTurn with
                             | Cross -> Nought
                             | Nought -> Cross
                             |_ -> game.GameTurn

            game

        let GameStart first size = 
            {GameSize = size; GameTurn = first; GameBoard = Array.empty<Player>;}

        let GameOutcome game     = raise (System.NotImplementedException("GameOutcome"))

        let FindBestMove game    = raise (System.NotImplementedException("FindBestMove"))

        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = raise (System.NotImplementedException("getCross"))
                member this.Nought with get()            = raise (System.NotImplementedException("getNought"))
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game move
                member this.FindBestMove(game)           = FindBestMove game