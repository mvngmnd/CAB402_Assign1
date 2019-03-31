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
            game.SetPiece(move.Row, move.Col);
            return game;
        }
        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }
        public Move FindBestMove(Game game)
        {
            throw new System.NotImplementedException("FindBestMove");
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

        public List<Player> LineAsPlayers(IEnumerable<System.Tuple<int, int>> line, Game game)
        {
            List<Player> lineObj = new List<Player>();
            foreach (Tuple<int, int> coords in line)
            {
                lineObj.Add(game.GetLocation(coords.Item1, coords.Item2));
            }
            return lineObj;
        }

        public TicTacToeOutcome<Player> LineOutcome(IEnumerable<System.Tuple<int,int>> line, Game game)
        {
            List<Player> listObj = LineAsPlayers(line, game);
            bool full = listObj.Count == game.Size;

            if (listObj.Contains(Nought) && listObj.Contains(Cross))
            {
                return TicTacToeOutcome<Player>.Draw;
            }
            if (listObj.TrueForAll((obj) => obj == Nought) && full)
            {
                return TicTacToeOutcome<Player>.NewWin(Nought, line);
            }
            if (listObj.TrueForAll((obj) => obj == Cross) && full)
            {
                return TicTacToeOutcome<Player>.NewWin(Cross, line);
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
    }
}