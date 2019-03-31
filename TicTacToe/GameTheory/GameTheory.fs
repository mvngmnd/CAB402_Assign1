namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =

            let rec MiniMax game perspective =
                NodeCounter.Increment()

                let maximizing = getTurn game = perspective
                let possibleMoves = moveGenerator game

                //Check if its game over or not more possible moves.
                if (gameOver game || Seq.isEmpty possibleMoves) then
                    (None, heuristic game perspective)
                else

                //For all possible moves, apply the move.
                let appliedMoves = seq{for move in possibleMoves do yield MiniMax (applyMove game move) perspective}

                //Associate a move with an applied move
                let zippedMoves = Seq.zip possibleMoves appliedMoves

                //Helper function, grabs the int from given nested tuple
                let heuristicOfNode (x:'Move * ('a option * int)) = snd(snd(x))

                //A function for comparing two given moves
                let comp (x:'Move * ('a option * int)) (y:'Move * ('a option * int)) = 
                    if (maximizing) then
                        if (heuristicOfNode(x) >= heuristicOfNode(y)) then
                            (fst(x), (Some (fst(x)), snd(snd(x))))
                        else
                            (fst(y), (Some (fst(y)), snd(snd(y))))
                    else
                        if (heuristicOfNode(y) >= heuristicOfNode(x)) then
                            (fst(x), (Some (fst(x)), snd(snd(x))))
                        else
                            (fst(y), (Some (fst(y)), snd(snd(y))))

                //Reduce down to the best using compare function.
                let bestMove = Seq.reduce (fun x y -> comp x y) zippedMoves
                (Some (fst(bestMove)), snd(snd(bestMove)))

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()

                //Get all possible moves
                let possibleMoves = moveGenerator oldState

                if (gameOver oldState || Seq.isEmpty possibleMoves) then
                    (None, heuristic oldState perspective)
                else
                    (None, heuristic oldState perspective)

            NodeCounter.Reset()
            MiniMax
