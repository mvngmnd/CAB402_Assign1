namespace QUT.CSharpTicTacToe
{
    public class Move : ITicTacToeMove
    {
        public int Row { set; get; }
        public int Col { set; get; } 

        public Move(int row, int col) {
            Row = row;
            Col = col;
        }

    }
}
