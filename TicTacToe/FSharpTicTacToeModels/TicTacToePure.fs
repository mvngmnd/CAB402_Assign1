 namespace QUT

    module FSharpPureTicTacToeModel =

        type Player = Nothing | Nought | Cross

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
                        |_ -> ""
                    else
                        ""

        let CreateMove row col = 
            {
                Row = row;
                Column = col
             }

        let ApplyMove (oldState:GameState) (move: Move) = 
            let newBoard = oldState.GameBoard.Add((move.Row, move.Column), oldState.GameTurn)
            let newPlayer = 
                match oldState.GameTurn with
                | Player.Cross -> Player.Nought
                | Player.Nought -> Player.Cross
                |_ -> oldState.GameTurn
            {GameSize = oldState.GameSize; GameBoard = newBoard; GameTurn = newPlayer}

        let Lines (size:int) : seq<seq<int*int>> = 

            //Try and have seq.append??

            let Rows : seq<seq<int*int>> = 
                seq {for x in 0 .. size-1 do yield seq{for y in 0 .. size-1 do yield (x,y)}}
            let Columns : seq<seq<int*int>> = 
                seq {for y in 0 .. size-1 do yield seq{for x in 0 .. size-1 do yield (x,y)}}
            let Diagonals =
                let left = seq{yield seq {for x in 0 .. size-1 do yield (x,x)}}
                let right = seq{yield seq {for x in 0 .. size-1 do yield (x, size-1 - (x%size))}}
                Seq.append left right
            Seq.append (Seq.append Rows Columns) Diagonals

        let lineAsPlayers (game:GameState) (line:seq<int*int>) = 
                seq{for coords in line do
                     if (game.GameBoard.ContainsKey(coords)) then
                        yield game.GameBoard.Item(coords) else yield Nothing}
                       
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> =
            let lineObj = lineAsPlayers game line

            let sumPlayer (player:Player) = 
                Seq.fold(fun acc elem -> if elem = player then acc+1 else acc) 0 lineObj
             
            if (sumPlayer(Nought) > 0 && sumPlayer(Cross) > 0) then
                Draw
            else if (sumPlayer(Nought) = game.GameSize) then
                Win(Nought, line)
            else if (sumPlayer(Cross) = game.GameSize) then
                Win(Cross, line)
            else
                Undecided

        let GameOutcome (game:GameState) : TicTacToeOutcome<Player> =
            let winningLines = Lines(game.GameSize)
            let gameOutcomes = seq{for line in winningLines do yield CheckLine game line}

            if (Seq.forall(fun elem -> elem = Draw) gameOutcomes) then
                Draw
            else if (Seq.forall(fun elem -> elem = Draw || elem = Undecided) gameOutcomes) then
                Undecided
            else
                Seq.find(fun elem -> elem <> Draw && elem <> Undecided) gameOutcomes
 
        let GameStart (firstPlayer:Player) (size:int) = {GameTurn = firstPlayer; GameSize = size; GameBoard = Map.empty<int*int, Player>}

        let TicTacToeHeuristic (game: GameState) (player:Player) =
            let outcome = GameOutcome game
            match outcome with
            | Win(x,_) -> if (x=player) then 1 else -1
            |_ -> 0

        let TicTacToeMoveGenerator (game:GameState) =
            let allPossibleMoves = seq{for x in 0 .. game.GameSize-1 do for y in 0 .. game.GameSize-1 do yield (x,y)}
            let moves = Seq.filter(fun x -> not(game.GameBoard.ContainsKey(x))) allPossibleMoves
            moves

        let TicTacToeGetTurn (game:GameState) = game.GameTurn

        let TicTacToeGameOver (game:GameState) = 
            match GameOutcome game with
            | Draw -> true
            | Win (_,_) -> true
            |_ -> false

        let TicTacToeApplyMove game (move:(int*int)) =
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
            override this.FindBestMove(game) =
                let coords = fst(MiniMax game).Value
                CreateMove (fst(coords)) (snd(coords))
                

        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = raise (System.NotImplementedException("FindBestMove"))