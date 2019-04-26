namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =

            let rec MiniMax game perspective =
                NodeCounter.Increment()

                if (gameOver game) then
                    (None, heuristic game perspective)
                else
                    moveGenerator game
                    //Apply the moves, and call minimax on the applied moves
                    |> Seq.map (fun move -> snd(MiniMax (applyMove game move) perspective))
                    |> Seq.toList
                    //Take all the score and find the best one, and then find the move associated
                    |> (fun scores -> if getTurn game = perspective then List.max scores else List.min scores
                                      |> (fun best -> List.findIndex(fun n -> n = best) scores, best))
                    |> (fun x -> (Some(Seq.item(fst(x)) (moveGenerator game)), (snd(x))))

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()

                let rec takeMax children row alpha beta =
                    let headMove = fst(List.head children)
                    let headGame = snd(List.head children)

                    let node = MiniMax alpha beta headGame perspective
                    let newAlpha = max alpha (snd(node))

                    //Add the node to our row list. This is used to keep track of nodes we've traversed
                    //in case we hit the end of a group of children without finding a good node.
                    let newRow = row @ [(headMove, node)]

                    if (newAlpha >= beta || children.Length = 1) then
                        //If we've hit the end without finding a good node.
                        if (children.Length = 1) then
                            if (row.IsEmpty) then
                                (Some(headMove), snd(node))
                            else
                                //Go find the maximum move of what we've visited.
                                List.max (List.map (fun x -> snd(snd(x))) newRow)
                                |> (fun maxValue -> List.findIndex (fun x -> snd(snd(x)) = maxValue) newRow)
                                |> (fun index -> List.item index newRow)
                                |> (fun maxMove -> (Some(fst(maxMove)), List.max (List.map (fun x -> snd(snd(x))) newRow)))
                        else
                            (Some(headMove), snd(node))
                    else
                        takeMax (List.tail children) newRow newAlpha beta 

                let rec takeMin children row alpha beta = 
                    let headMove = fst(List.head children)
                    let headGame = snd(List.head children)

                    let node = MiniMax alpha beta headGame perspective
                    let newBeta = min beta (snd(node))

                    //Add the node to our row list. This is used to keep track of nodes we've traversed
                    //in case we hit the end of a group of children without finding a good node.
                    let newRow = row @ [(headMove, node)]

                    if (newBeta <= alpha || children.Length = 1) then
                        //If we've hit the end without finding a good node.
                        if (children.Length = 1) then
                            if (row.IsEmpty) then
                                (Some(headMove), snd(node))
                            else
                                //Go find the maximum move of what we've visited.
                                List.min (List.map (fun x -> snd(snd(x))) newRow)
                                |> (fun minValue -> List.findIndex (fun x -> snd(snd(x)) = minValue) newRow)
                                |> (fun index -> List.item index newRow)
                                |> (fun minMove -> (Some(fst(minMove)), List.min (List.map (fun x -> snd(snd(x))) newRow)))
                        else
                            (Some(headMove), snd(node))
                    else
                        takeMin (List.tail children) newRow alpha newBeta 

                if (gameOver oldState) then
                    (None, heuristic oldState perspective)
                else
                    moveGenerator oldState
                        //Apply the moves
                        |> Seq.map (fun move -> applyMove oldState move)
                        |> Seq.toList
                        //Zip each applied move to its associated move
                        |> List.zip (Seq.toList(moveGenerator oldState))
                        |> (fun x -> 
                                if getTurn oldState = perspective then
                                    takeMax x List.Empty alpha beta
                                else
                                    takeMin x List.Empty alpha beta)
                        
            NodeCounter.Reset()
            MiniMax
