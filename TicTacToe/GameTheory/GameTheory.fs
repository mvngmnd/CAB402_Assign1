namespace QUT

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
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()

                let rec takeMax children row alpha beta =
                    let head = List.head children
                    let headGame = snd(head)

                    let v = MiniMax alpha beta headGame perspective

                    let newRow = row @ [(fst(head), v)]
                    let new_a = max alpha (snd(v))

                    if (new_a >= beta || children.Length = 1) then
                        if (children.Length = 1) then
                            if (row.IsEmpty) then
                                (Some(fst(head)), snd(v))
                            else
                            let ints = List.map (fun x -> snd(snd(x))) newRow
                            let maxInt = List.max ints
                            let index = List.findIndex (fun x -> snd(snd(x)) = maxInt) newRow
                            let i = List.item index newRow
                            (Some(fst(i)), maxInt)
                        else
                            (Some(fst(head)), snd(v))
                    else
                        takeMax (List.tail children) newRow new_a beta 

                let rec takeMin children row alpha beta = 
                    let head = List.head children
                    let headGame = snd(head)

                    let v = MiniMax alpha beta headGame perspective
                    let newRow = row @ [(fst(head), v)]
                    let new_b = min beta (snd(v))

                    if (new_b <= alpha || children.Length = 1) then
                        if (children.Length = 1) then
                            if (row.IsEmpty) then
                                (Some(fst(head)), snd(v))
                            else

                            let ints = List.map (fun x -> snd(snd(x))) newRow
                            let minInt = List.min ints
                            let index = List.findIndex (fun x -> snd(snd(x)) = minInt) newRow
                            let i = List.item index newRow
                            (Some(fst(i)), minInt)
                        else
                            (Some(fst(head)), snd(v))
                    else
                        takeMin (List.tail children) newRow alpha new_b 

                if (gameOver oldState) then
                    (None, heuristic oldState perspective)
                else
                    let bestMove = 
                        moveGenerator oldState
                        |> Seq.map (fun move -> applyMove oldState move)
                        |> Seq.toList
                        |> List.zip (Seq.toList(moveGenerator oldState))
                        |> (fun x -> 
                                if getTurn oldState = perspective then
                                    takeMax x List.Empty alpha beta
                                else
                                    takeMin x List.Empty alpha beta)
                    bestMove
            NodeCounter.Reset()
            MiniMax
