namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =

            let rec MiniMax game perspective =
                NodeCounter.Increment()

                let moves = 
                    seq{for move in moveGenerator game do yield move}

                if (gameOver game || Seq.isEmpty moves) then
                    (None, heuristic game perspective)

                else

                let appliedMoves = 
                    seq{for move in moves do 
                            yield MiniMax (applyMove game move) perspective}

                let zippedMoves = Seq.zip moves appliedMoves

                let max = getTurn game = perspective

                let comp (x:'Move * ('a option * int)) (y:'Move * ('a option * int)) (max:bool) = 
                    let xheuristic = snd(snd(x))
                    let yheuristic = snd(snd(x))
                    let xmove = fst(x)
                    let ymove = fst(y)

                    if (max) then
                        if (xheuristic >= yheuristic) then
                            (xmove, (Some xmove, xheuristic))
                        else
                            (ymove, (Some xmove, yheuristic))
                    else
                        if (xheuristic < yheuristic) then
                            (xmove, (Some ymove, xheuristic ))
                        else
                            (ymove, (Some ymove, yheuristic))
                        

                let best = Seq.reduce (fun x y -> comp x y max) zippedMoves
                (Some (fst(best)), snd(snd(best)))

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
