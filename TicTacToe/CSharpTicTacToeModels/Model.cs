using System.Collections.Generic;
using System.Linq;
using System;

namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.Cross;
        public Player Nought => Player.Nought;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }
        public Game ApplyMove(Game game, Move move)
        {
            Game copy = new Game(game.Size, 0);
            game.Board.CopyTo(copy.Board, 0);
            copy.SetTurn(game.Turn);
            copy.SetPiece(move.Row, move.Col);
            copy.ChangeTurn();
            return copy;
        }
        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }
        public int Heuristic(Game game, Player player)
        {
            TicTacToeOutcome<Player> outcome = GameOutcome(game);
            if (outcome.IsWin)
            {
                return ((outcome as TicTacToeOutcome<Player>.Win).winner == player) ? 1 : -1;
            } 
            else
            {
                return 0;
            }
        }
        public Player GetTurn (Game game)
        {
            return game.Turn;
        }
        public bool GameOver (Game game) 
        {
            return !(GameOutcome(game).IsUndecided);
        }
        public List<List<Tuple<int,int>>> Lines (int size)
        {
            List<List<Tuple<int, int>>> lines = new List<List<Tuple<int, int>>>();
            List<Tuple<int, int>> line = new List<Tuple<int, int>>();

            //Do the columns
            for (int x = 0; x < size; x++)
            {
                for (int y = 0; y < size; y++)
                {
                    line.Add(new Tuple<int, int>(x,y));
                }
                lines.Add(line);
                line = new List<Tuple<int, int>>();
            }
            //Do the rows
            for (int y = 0; y < size; y++)
            {
                for (int x = 0; x < size; x++)
                {
                    line.Add(new Tuple<int, int>(x, y));
                }
                lines.Add(line);
                line = new List<Tuple<int, int>>();
            }
            //Do the left diagonal
            for (int x = 0; x < size; x++)
            {
                line.Add(new Tuple<int, int>(x, x));
            }
            lines.Add(line);
            line = new List<Tuple<int, int>>();

            //Do the right diagonal
            for (int x = 0; x < size; x++)
            {
                line.Add(new Tuple<int, int>(x, size - 1 - (x % size)));
            }
            lines.Add(line);
            return lines;
        }
        public List<Player> LineAsPlayers(List<Tuple<int,int>> line, Game game)
        {
            List<Player> lineObj = new List<Player>();
            foreach (Tuple<int,int> coords in line)
            {
                lineObj.Add(game.GetLocation(coords.Item1, coords.Item2));
            }
            return lineObj;
        }
        public TicTacToeOutcome<Player> LineOutcome(List<Tuple<int, int>> line, Game game)
        {
            List<Player> listObj = LineAsPlayers(line, game);

            if (listObj.Contains(Nought) && listObj.Contains(Cross))
            {
                return TicTacToeOutcome<Player>.Draw;
            }
            if (listObj.TrueForAll((obj) => obj == Nought))
            {
                return TicTacToeOutcome<Player>.NewWin(Nought, line.AsEnumerable());
            }
            if (listObj.TrueForAll((obj) => obj == Cross))
            {
                return TicTacToeOutcome<Player>.NewWin(Cross, line.AsEnumerable());
            }
            return TicTacToeOutcome<Player>.Undecided;
        }
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            List<TicTacToeOutcome<Player>> boardOutcomes = new List<TicTacToeOutcome<Player>>();
            foreach (var line in Lines(game.Size))
            {
                boardOutcomes.Add(LineOutcome(line, game));
            }

            if (boardOutcomes.TrueForAll((obj) => obj == TicTacToeOutcome<Player>.Draw))
            {
                return TicTacToeOutcome<Player>.Draw;
            }

            if (boardOutcomes.TrueForAll((obj) => obj == TicTacToeOutcome<Player>.Draw || obj == TicTacToeOutcome<Player>.Undecided))
            {
                return TicTacToeOutcome<Player>.Undecided;
            }

            return boardOutcomes.Find((obj) => obj != TicTacToeOutcome<Player>.Draw && obj != TicTacToeOutcome<Player>.Undecided);
        }
        public Game GameStart(Player first, int size)
        {
            return new Game(size, first);
        }
        public List<Move> MoveGenerator(Game game)
        {
            List<Move> moves = new List<Move>();

            for (int x = 0; x < game.Size; x++)
            {
                for (int y = 0; y < game.Size; y++)
                {
                    if (game.GetLocation(x, y) == 0)
                    {
                        moves.Add(new Move(x, y));
                    }
                }
            }

            return moves;
        }
        public Move FindBestMove(Game game)
        {
            int Alpha = -1;
            int Beta = 1;
            Player perspective = game.Turn;

            (Move, int) TakeMin(List<(Move, Game)> children, List<(Move, int)> row, int alpha, int beta)
            {
                var head = children[0];
                var tail = new List<(Move, Game)>(children.Skip(1));

                var headMove = head.Item1;
                var headGame = head.Item2;

                var node = AlphaBeta(headGame, alpha, beta);
                var nodeHeuristic = node.Item2;

                row.Add((headMove, nodeHeuristic));
                var newBeta = Math.Min(beta, nodeHeuristic);

                if (newBeta <= alpha)
                {
                    return (headMove, nodeHeuristic);
                }
                if (children.Count == 1)
                {
                    if (row.Count == 0)
                    {
                        return (headMove, nodeHeuristic);
                    }
                    else
                    {
                        List<int> listOfHeuristics = row.Select(x => x.Item2).ToList();
                        int minHeuristic = listOfHeuristics.Min();
                        int index = listOfHeuristics.FindIndex(x => x == minHeuristic);
                        return row[index];
                    }
                }

                return TakeMin(tail, row, alpha, newBeta);
            }

            (Move, int) TakeMax(List<(Move, Game)> children, List<(Move, int)> row, int alpha, int beta)
            {
                (Move,Game) head = children[0];
                List<(Move,Game)> tail = new List<(Move, Game)>(children.Skip(1));

                Move headMove = head.Item1;
                Game headGame = head.Item2;

                (Move,int) node = AlphaBeta(headGame, alpha, beta);
                int nodeHeuristic = node.Item2;
                row.Add((headMove, nodeHeuristic));
                int newAlpha = Math.Max(alpha, nodeHeuristic);

                if (newAlpha >= beta)
                {
                        return (headMove, nodeHeuristic);
                }
                if (children.Count == 1)
                {
                    if (row.Count == 0)
                    {
                        return (headMove, nodeHeuristic);
                    }
                    else
                    {
                        List<int> listOfHeuristics = row.Select(x => x.Item2).ToList();
                        int maxHeuristic = listOfHeuristics.Max();
                        int index = listOfHeuristics.FindIndex(x => x == maxHeuristic);
                        return row[index];
                    }
                }
                else
                {
                    return TakeMax(tail, row, newAlpha, beta);
                }
            }

            (Move, int) AlphaBeta(Game node, int alpha, int beta)
            {
                NodeCounter.Increment();
                if (GameOver(node))
                {
                    return (null, Heuristic(node, perspective));
                }

                List<Move> moves = MoveGenerator(node);

                List<Game> appliedMoves = moves.Select(x => ApplyMove(node, x)).ToList();

                List<(Move, Game)> children = moves.Zip(appliedMoves, (move, appliedMove) => (move, appliedMove)).ToList();

                if (perspective == GetTurn(node))
                {
                    return (TakeMax(children, new List<(Move, int)>(), alpha, beta));
                }

                return (TakeMin(children, new List<(Move, int)>(), alpha, beta));
            }

            NodeCounter.Reset();
            return AlphaBeta(game, Alpha, Beta).Item1;
        }
    }
}