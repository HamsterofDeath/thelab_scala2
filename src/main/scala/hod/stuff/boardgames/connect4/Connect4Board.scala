package hod.stuff.boardgames.connect4

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

import hod.stuff.boardgames.logic.{AutoPlay, BoardPlayer, BoardPrinter, BoardRating, BoardState, GameContext, Move, MoveCacheSupport,
  MovesExhausted, MutableBoard, OutcomeNotDetermined, WinnerDetermined}

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
  def hasAnyPlayerWon: Boolean = winner.isDefined

  def numberOfMovesMade: Int = movesPlayed

  def maxRemainingMoves = maxMoves - numberOfMovesMade

  def stateAsBits: (Long, Long) = (bluePlayerBits, redPlayerBits)

  def nextPossibleMoveCoordinates =
    validMoves
      .map(_.column)
      .map { col =>
        col -> filledUpTo(col)
      }
      .filter(e => e._2 < height)

  def isTopReached(col: Int) = filledUpTo(col) >= height

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
    Array(Blue1, Blue2, Blue3, Blue4, Blue5, Blue6, Blue7)
  private val allRedMoves  =
    Array(Red1, Red2, Red3, Red4, Red5, Red6, Red7)
  private val noMoves      = Array.empty[PlaceCoin]

  private val width    = 7
  private val height   = 6
  private val maxMoves = width * height

  private var bluePlayerBits            = 0L
  private var redPlayerBits             = 0L
  private val filledUpTo                = Array.fill(width)(0)
  private var bitsForHeightLimitReached = 0
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

  private def countInLine(bits: Long,
                          x: Int,
                          y: Int,
                          xShift: Int,
                          yShift: Int) = {
    def inBounds(x: Int, y: Int) = x >= 0 && y >= 0 && x < width && y < height

    var found = 0
    var myX   = x
    var myY   = y

    var wasLastFieldSet = false
    do {
      myX += xShift
      myY += yShift
      wasLastFieldSet = inBounds(myX, myY) && isSet(bits, myX, myY)
      if (wasLastFieldSet) {
        found += 1
      }
    }
    while (wasLastFieldSet)
    found
  }

  def countMaxOwnedInLine(blue: Boolean, x: Int, y: Int): Int = {
    val bits = if (blue) bluePlayerBits else redPlayerBits

    def leftRight =
      1 + countInLine(bits, x, y, -1, 0) + countInLine(bits, x, y, 1, 0)

    def upDown =
      1 + countInLine(bits, x, y, 0, -1) + countInLine(bits, x, y, 0, 1)

    def slash =
      1 + countInLine(bits, x, y, -1, -1) + countInLine(bits, x, y, 1, 1)

    def backSlash =
      1 + countInLine(bits, x, y, -1, 1) + countInLine(bits, x, y, 1, -1)

    leftRight max upDown max slash max backSlash
  }

  private val requiredInLine = 4

  private def isGameOver(blue: Boolean, x: Int, y: Int): Boolean = {
    countMaxOwnedInLine(blue, x, y) >= requiredInLine
  }

  private def checkGameOver(firstPlayer: Boolean, x: Int, y: Int) = {
    if (isGameOver(firstPlayer, x, y)) {
      if (firstPlayer) {
        winner = blueSet
      } else {
        winner = redSet
      }
    }
  }

  private def evalBitForHeightLimitReached(x: Int) =
    if (filledUpTo(x) >= height) 1 << x else 0

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
    bitsForHeightLimitReached |= evalBitForHeightLimitReached(x)
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
    bitsForHeightLimitReached &= ~(1 << x)
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
      if (winner.isEmpty) {
        if (currentPlayerIsMaximizing) {
          allBlueMoves
        } else {
          allRedMoves
        }
      } else {
        noMoves
      }
    }

    val bitsCopy = bitsForHeightLimitReached
    moves.iterator.filter { move =>
      val check = 1 << move.column
      (bitsCopy & check) != check
    }
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

  override def printMove(move: PlaceCoin, board: Connect4Board): String = {
    s"Player ${if (move.isBlue) "Blue" else "Red"} in column ${move.column + 1}"
  }
  override def printBoard(board: Connect4Board): String = {
    "1234567\n" +
    (0 to 5 map { row =>
      (0 to 6 map { col =>
        board.fieldState(col, 5 - row) match {
          case Some(true) => colored("X", Colors.ANSI_BLUE)
          case Some(false) => colored("O", Colors.ANSI_RED)
          case None => " "
        }

      }).mkString
    }).mkString("\n") + "\n1234567"
  }
}

class Connect4CacheSupport extends MoveCacheSupport[PlaceCoin, Connect4Board] {
  private val cache = mutable.HashMap.empty[(Long, Long), Int]
  override def clear(): Unit = cache.clear()
  override def isCacheSupported(depth: Int): Boolean = depth <= 14
  override def store(b: Connect4Board, rating: Int): Unit =
    cache.put(b.stateAsBits, rating)
  override def hasRatingStored(b: Connect4Board): Boolean =
    cache.contains(b.stateAsBits)
  override def getRating(b: Connect4Board): Int = cache(b.stateAsBits)
}

object Connect4Rating extends BoardRating[PlaceCoin, Connect4Board] {

  override def rate(situation: Connect4Board): Int = {
    val highPrio =
      if (situation.hasBluePlayerWon)
        100 + situation.maxRemainingMoves
      else if (situation.hasRedPlayerWon)
        -100 - situation.maxRemainingMoves
      else 0

    val lowPrio = 0
    highPrio + lowPrio
  }
}

object Connect4Board {
  def main(args: Array[String]): Unit = {
    val board = new Connect4Board
    val ctx   = new GameContext[PlaceCoin, Connect4Board](
      board,
      50,
      maxLeafEvals = Some(1200000),
      Connect4Rating,
      Connect4Printer,
      true,
      false,
      Some(new Connect4CacheSupport)
    )

    def askInput(context: GameContext[PlaceCoin, Connect4Board]) = {
      var success = Option.empty[PlaceCoin]
      while (success.isEmpty) {
        val move = scala.io.StdIn.readLine()
        Try {
          Some(move.toInt)
            .filterNot(e => context.board.isTopReached(e - 1))
            .get match {
            case 1 => Blue1
            case 2 => Blue2
            case 3 => Blue3
            case 4 => Blue4
            case 5 => Blue5
            case 6 => Blue6
            case 7 => Blue7
          }
        } match {
          case Failure(_) => println("No! Again!")
          case Success(value) =>
            success = Some(value)
        }
      }
      success.get
    }

    AutoPlay.playTwoPlayerGame[PlaceCoin, Connect4Board](
      ctx,
      firstPlayer = (context: GameContext[PlaceCoin, Connect4Board]) => {
        println("Your move?")
        askInput(context)

      },
      //firstPlayer = BoardPlayer.autoPlayer,
      secondPlayer = BoardPlayer.autoPlayer
    )
  }
}
