package hod.stuff.boardgames.connect4

import hod.stuff.boardgames.logic.{AutoPlay, BoardPrinter, BoardRating, BoardState, GameContext, Move, MovesExhausted, MutableBoard,
  OutcomeNotDetermined, WinnerDetermined}

sealed trait PlaceCoin extends Move {
  def isBlue: Boolean
  def column: Int
}

trait BluePlayerMove extends PlaceCoin {
  override def isBlue: Boolean = true
}

case object Blue1 extends BluePlayerMove {
  override def column: Int = 0
}
case object Blue2 extends BluePlayerMove {
  override def column: Int = 1
}
case object Blue3 extends BluePlayerMove {
  override def column: Int = 2
}
case object Blue4 extends BluePlayerMove {
  override def column: Int = 3
}
case object Blue5 extends BluePlayerMove {
  override def column: Int = 4
}
case object Blue6 extends BluePlayerMove {
  override def column: Int = 5
}
case object Blue7 extends BluePlayerMove {
  override def column: Int = 6
}

trait RedPlayerMove extends PlaceCoin {
  override def isBlue: Boolean = false
}

case object Red1 extends RedPlayerMove {
  override def column: Int = 0
}
case object Red2 extends RedPlayerMove {
  override def column: Int = 1
}
case object Red3 extends RedPlayerMove {
  override def column: Int = 2
}
case object Red4 extends RedPlayerMove {
  override def column: Int = 3
}
case object Red5 extends RedPlayerMove {
  override def column: Int = 4
}
case object Red6 extends RedPlayerMove {
  override def column: Int = 5
}
case object Red7 extends RedPlayerMove {
  override def column: Int = 6
}

class Connect4Board extends MutableBoard[PlaceCoin] {
  def hasBluePlayerWon: Boolean = winner == blueSet
  def hasRedPlayerWon: Boolean = winner == redSet

  def fieldState(col: Int, row: Int): Option[Boolean] = {
    if (isSet(bluePlayerBits, col, row)) {
      blueSet
    } else if (isSet(redPlayerBits, col, row)) {
      redSet
    } else {
      None
    }
  }

  private val allBlueMoves =
    List(Blue1, Blue2, Blue3, Blue4, Blue5, Blue6, Blue7)
  private val allRedMoves  = List(Red1, Red2, Red3, Red4, Red5, Red6, Red7)

  private val width    = 7
  private val height   = 6
  private val maxMoves = width * height

  private var bluePlayerBits            = 0L
  private var redPlayerBits             = 0L
  private val filledUpTo                = Array.fill(width)(0)
  private var currentPlayerIsMaximizing = true
  private val noWinner                  = Option.empty[Boolean]
  private val blueSet                   = Some(true)
  private val redSet                    = Some(false)
  private var winner                    = noWinner
  private var movesPlayed               = 0

  private def isSet(bits: Long, x: Int, y: Int) = {
    val check = nthBitSet(x, y)
    (check & bits) == check
  }

  def isGameOver(bits: Long, x: Int, y: Int): Boolean = {
    def inBounds(x: Int, y: Int) = x >= 0 && y >= 0 && x < width && y < height

    def countSet(xShift: Int, yShift: Int) = {
      var found = 0
      var myX   = x
      var myY   = y

      var wasLastFieldSet = false
      do {
        myX += xShift
        myY += yShift
        wasLastFieldSet = isSet(bits, myX, myY)
        if (wasLastFieldSet) {
          found += 1
        }
      }
      while (wasLastFieldSet && inBounds(x, y))
      found
    }

    def leftRight = 1 + countSet(-1, 0) + countSet(1, 0)

    def upDown = 1 + countSet(0, -1) + countSet(0, 1)

    def slash = 1 + countSet(-1, -1) + countSet(1, 1)

    def backSlash = 1 + countSet(-1, 1) + countSet(1, -1)

    leftRight >= 4 ||
    upDown >= 4 ||
    slash >= 4 ||
    backSlash >= 4
  }

  private def checkGameOver(firstPlayer: Boolean, x: Int, y: Int) = {
    val bits = if (firstPlayer) bluePlayerBits else redPlayerBits
    if (isGameOver(bits, x, y)) {
      winner = if (firstPlayer) blueSet else redSet
    }
  }

  private def set(firstPlayer: Boolean, x: Int, y: Int) = {
    val oneBitSet = {
      nthBitSet(x, y)
    }
    if (firstPlayer) {
      bluePlayerBits |= oneBitSet
    } else {
      redPlayerBits |= oneBitSet
    }
    filledUpTo(x) += 1
    checkGameOver(firstPlayer, x, y)
    movesPlayed += 1
    currentPlayerIsMaximizing = !currentPlayerIsMaximizing
  }

  private def unset(firstPlayer: Boolean, x: Int, y: Int) = {
    val oneBitSet = {
      ~nthBitSet(x, y)
    }
    if (firstPlayer) {
      bluePlayerBits &= oneBitSet
    } else {
      redPlayerBits &= oneBitSet
    }
    filledUpTo(x) -= 1
    winner = noWinner
    movesPlayed -= 1
    currentPlayerIsMaximizing = !currentPlayerIsMaximizing
  }

  private def nthBitSet(x: Int, y: Int) = {
    val n = x + y * width
    1L << n
  }

  override def undo(move: PlaceCoin): Unit = {
    unset(move.isBlue, move.column, filledUpTo(move.column) - 1)
  }

  override def validMoves: Iterator[PlaceCoin] = {
    val moves = {
      if (currentPlayerIsMaximizing) {
        allBlueMoves
      } else {
        allRedMoves
      }
    }
    moves.filter { move =>
      filledUpTo(move.column) < height
    }.iterator
  }

  override def applied(move: PlaceCoin): Unit = {
    val height = filledUpTo(move.column)
    set(move.isBlue, move.column, height)
  }

  override def boardState: BoardState = {
    winner match {
      case Some(_) => WinnerDetermined
      case None if movesPlayed < maxMoves => OutcomeNotDetermined
      case None => MovesExhausted
    }
  }

  override def isTurnOfMaximizingPlayer: Boolean = currentPlayerIsMaximizing
}

object Connect4Printer extends BoardPrinter[PlaceCoin, Connect4Board] {
  override def printBoard(board: Connect4Board): String = {
    (0 to 6 map { row =>
      (0 to 7 map { col =>
        board.fieldState(col, 6 - row) match {
          case Some(true) => "X"
          case Some(false) => "O"
          case None => " "
        }

      }).mkString
    }).mkString("\n")
  }
}

object Connect4Rating extends BoardRating[PlaceCoin, Connect4Board] {
  override def rate(situation: Connect4Board): Int = {
    if (situation.hasBluePlayerWon) 1
    else if (situation.hasRedPlayerWon) -1
    else 0
  }
}

object Connect4Board {
  def main(args: Array[String]): Unit = {
    val board = new Connect4Board
    board.applied(Blue1)
    board.undo(Blue1)
    AutoPlay.playTwoPlayerGame(
      new GameContext[PlaceCoin, Connect4Board](
        board,
        4,
        Connect4Rating,
        Connect4Printer,
        true,
        false,
        false
      )
    )
  }
}
