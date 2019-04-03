namespace QUT

    //generate the scores as a single list, then just do an index thing, using maxby snd thing

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =

            let rec MiniMax game perspective =
                NodeCounter.Increment()

                if (gameOver game) then
                    (None, heuristic game perspective)
                else

                    let bestScore = moveGenerator game
                                    |> Seq.map (fun move -> snd(MiniMax (applyMove game move) perspective))
                                    |> Seq.toList
                                    |> (fun scores -> if getTurn game = perspective then List.max scores else List.min scores
                                                      |> (fun best -> List.findIndex(fun n -> n = best) scores, best))

                    (Some(Seq.item(fst(bestScore)) (moveGenerator game)), (snd(bestScore)))

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()

                //alpha beta could possibly seperate calls of the the function?

                //old state seems to suggest something

                //alpha could be an implementation that does a return
                //straight away if its less than its parent?

                if (gameOver oldState) then
                    (None, heuristic oldState perspective)
                else
                    (None, heuristic oldState perspective)

            NodeCounter.Reset()
            MiniMax
