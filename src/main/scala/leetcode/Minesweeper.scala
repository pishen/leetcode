package leetcode

object Minesweeper {
  def run(board: Array[Array[Char]], click: Array[Int]): Array[Array[Char]] = {
    def re(board: Array[Array[Char]]): Array[Array[Char]] = {
      val newBoard = board.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case (cell, j) =>
          def countMine() = {
            val res = for {
              i2 <- i - 1 to i + 1
              j2 <- j - 1 to j + 1
            } yield {
              board.lift(i2).flatMap(_.lift(j2))
                .map(c => if (c == 'M' || c == 'X') 1 else 0)
                .getOrElse(0)
            }
            res.sum
          }
          
          def blankNearby() = {
            val indices = for {
              i2 <- i - 1 to i + 1
              j2 <- j - 1 to j + 1
              if !(i2 == i && j2 == j)
            } yield {
              (i2, j2)
            }
            indices.exists { case (i2, j2) =>
              board.lift(i2).flatMap(_.lift(j2)).filter(_ == 'B').isDefined
            }
          }
          
          if (i == click(0) && j == click(1)) {
            if (cell == 'M') {
              'X'
            } else if (cell == 'E') {
              val mineCount = countMine()
              if (mineCount == 0) 'B' else mineCount.toString.head
            } else {
              cell
            }
          } else {
            if (blankNearby()) {
              val mineCount = countMine()
              if (mineCount == 0) 'B' else mineCount.toString.head
            } else {
              cell
            }
          }
        }
      }
      val different = board.zip(newBoard).exists {
        case (rowA, rowB) => rowA.zip(rowB).exists { case (a, b) => a != b }
      }
      if (different) re(newBoard) else newBoard
    }
    
    re(board)
  }
}
