namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =

            let rec MiniMax game perspective =
                NodeCounter.Increment()

                if (gameOver game) then
                    (None, heuristic game perspective)
                else
                    //The set of all possible moves for our current state
                    let moves = seq{for move in moveGenerator game do
                                    let newState = applyMove game move
                                    yield MiniMax newState (getTurn newState)
                                   }

                    if (perspective = getTurn game) then
                        Seq.reduce(fun (x:(Option<'Move>*int)) (y:(Option<'Move>*int)) -> if (snd x > snd y) then y else x) moves
                    else
                        Seq.reduce(fun (x:(Option<'Move>*int)) (y:(Option<'Move>*int)) -> if (snd x < snd y) then y else x) moves
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
