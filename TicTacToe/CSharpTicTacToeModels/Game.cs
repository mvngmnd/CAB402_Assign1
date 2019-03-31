using System.Collections.Generic;
using System;
using System.Collections;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {

        public int Size { set; get; }
        private readonly Player[] _board;
        private Player _turn;

        public Game(int size, Player firstTurn) {
            Size = size;
            _turn = firstTurn;
            _board = new Player[size * Size];
        }


        public Player Turn => _turn;
        public void ChangeTurn()
        {
            switch (_turn)
            {
                case Player.Cross:
                    _turn = Player.Nought;
                    break;
                case Player.Nought:
                    _turn = Player.Cross;
                    break;
            }
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
            ChangeTurn();
        }
    }
}