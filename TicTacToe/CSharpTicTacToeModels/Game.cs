using System.Collections.Generic;
using System;
using System.Collections;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public int Size { set; get; }
        private Player[] _board;
        private Player _turn;

        public Player[] Board
        {
            get
            {
                return _board;
            }
            set
            {
                _board = value;
            }
        }

        public Game(int size, Player firstTurn) {
            Size = size;
            _turn = firstTurn;
            _board = new Player[size * Size];
        }

        public Player Turn => _turn;
        public void ChangeTurn()
        {
            _turn = (_turn == Player.Cross) ? Player.Nought : Player.Cross;
        }

        public Player GetLocation(int row, int col)
        {
            return _board[row * Size + col];
        }

        public string getPiece(int row, int col)
        {
            switch (GetLocation(row, col))
            {
                case Player.Cross:
                    return "X";
                case Player.Nought:
                    return "O";
                default:
                    return "";
            }
        }

        public void SetPiece(int row, int col)
        {
            _board[row * Size + col] = Turn;
        }

        public void SetTurn(Player player)
        {
            _turn = player;
        }
    }
}