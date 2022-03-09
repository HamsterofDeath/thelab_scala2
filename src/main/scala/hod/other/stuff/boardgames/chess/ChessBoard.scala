package hod.other.stuff.boardgames.chess

import hod.other.stuff.boardgames.logic.{AutoPlay, BoardPrinter, BoardRating, BoardState, GameContext, Move, MutableBoard, OutcomeNotDetermined,
  WinnerDetermined}

abstract case class Piece(value: Int) {
  def pieceName = getClass.getSimpleName

  def targetsOn(field: FieldWrapper, currentPlayer: Player, pieceLocation: Location): Iterator[ChessMove]

}
class FieldWrapper(field: Array[Array[Option[PieceOfPlayer]]]) {
  def locationToMove(from: Location, to: Location): ChessMove = {
    ChessMove(from, to, field(to.x)(to.y))
  }

  def isValidAndFreeOrOccupiedBy(location: Location, owner: Player): Boolean = {
    isValid(location) && (isFree(location) || isOccupiedBy(location, owner))
  }

  private def pathUntil(pieceLocation: Location, xShift: Int, yShift: Int, canAttack: Player, infinite: Boolean): Iterator[Location] = {
    var cursor = pieceLocation

    def shifted = {
      cursor = cursor.moved(xShift, yShift)
      cursor
    }

    val endless = {
      var firstContact = false
      Iterator.continually(shifted)
              .takeWhile { locationToCheck =>
                if (isValid(locationToCheck) && !firstContact) {
                  if (isFree(locationToCheck)) {
                    true
                  } else {
                    val moveOn = isOccupiedBy(locationToCheck, canAttack)
                    if (moveOn) {
                      firstContact = true
                    }
                    moveOn
                  }
                } else {
                  false
                }
              }
    }
    if (infinite) {
      endless
    } else {
      endless.take(1)
    }
  }

  def pathsFrom(pieceLocation: Location, lrud: Boolean, diagonal: Boolean, infiniteRange: Boolean, pieceOwner: Player): Iterator[Location] = {
    val enemy           = pieceOwner.otherPlayer
    val leftRightUpDown = {
      if (lrud) {
        pathUntil(pieceLocation, -1, 0, enemy, infiniteRange) ++
        pathUntil(pieceLocation, 1, 0, enemy, infiniteRange) ++
        pathUntil(pieceLocation, 0, -1, enemy, infiniteRange) ++
        pathUntil(pieceLocation, 0, 1, enemy, infiniteRange)
      } else {
        Iterator.empty
      }
    }
    val rot45           = {
      if (diagonal) {
        pathUntil(pieceLocation, -1, -1, enemy, infiniteRange) ++
        pathUntil(pieceLocation, 1, -1, enemy, infiniteRange) ++
        pathUntil(pieceLocation, -1, 1, enemy, infiniteRange) ++
        pathUntil(pieceLocation, 1, 1, enemy, infiniteRange)
      } else {
        Iterator.empty
      }
    }

    leftRightUpDown ++ rot45
  }

  def isOccupiedBy(location: Location, player: Player): Boolean = {
    val op = field(location.x)(location.y)
    op.isDefined && op.get.owner == player
  }

  def isValidAndOccupiedBy(location: Location, player: Player): Boolean = {
    isValid(location) && isOccupiedBy(location, player)
  }

  def isValid(location: Location): Boolean = {
    location.x >= 0 &&
    location.y >= 0 &&
    location.x <= 7 &&
    location.y <= 7
  }

  def isFree(location: Location): Boolean = {
    field(location.x)(location.y).isEmpty
  }

  def isFreeAndValid(location: Location): Boolean = {
    isValid(location) && isFree(location)
  }

}
object Pawn extends Piece(1) {
  override def targetsOn(field: FieldWrapper,
                         currentPlayer: Player,
                         pieceLocation: Location): Iterator[ChessMove] = {
    def pawnMoves(down: Boolean, startY: Int) = {
      val directionFactor = if (down) 1 else -1
      val oneForward      = pieceLocation.moved(0, 1 * directionFactor)

      def twoForward = pieceLocation.moved(0, 2 * directionFactor)

      val moveOneForward = {
        if (field.isFreeAndValid(oneForward)) {
          Iterator.single(ChessMove(pieceLocation, oneForward, None))
        } else {
          Iterator.empty
        }
      }
      val moveTwoForward = {
        if (pieceLocation.y == startY) {
          if (field.isFree(twoForward)) {
            Iterator.single(ChessMove(pieceLocation, twoForward, None))
          } else {
            Iterator.empty
          }
        } else {
          Iterator.empty
        }
      }

      def isAttackable(target: Location) = {
        field.isValidAndOccupiedBy(target, currentPlayer.otherPlayer)
      }

      val attackLeft = {
        val target = pieceLocation.moved(-1, 1 * directionFactor)
        if (isAttackable(target)) {
          Iterator.single(field.locationToMove(pieceLocation, target))
        } else {
          Iterator.empty
        }
      }

      val attackRight = {
        val target = pieceLocation.moved(1, 1 * directionFactor)
        if (isAttackable(target)) {
          Iterator.single(field.locationToMove(pieceLocation, target))
        } else {
          Iterator.empty
        }
      }

      moveOneForward ++ moveTwoForward ++ attackLeft ++ attackRight

    }

    currentPlayer match {
      case White => pawnMoves(true, 1)
      case Black => pawnMoves(false, 6)
    }

  }
}
object Horse extends Piece(3) {
  private val relativeTargets = {
    List(
      (1, 2),
      (1, -2),
      (2, 1),
      (2, -1),
      (-1, 2),
      (-1, -2),
      (-2, 1),
      (-2, -1)
    )
  }
  override def targetsOn(field: FieldWrapper, currentPlayer: Player,
                         pieceLocation: Location): Iterator[ChessMove] = {
    relativeTargets.iterator.map { shift =>
      pieceLocation.moved(shift._1, shift._2)
    }.filter(field.isValidAndFreeOrOccupiedBy(_, currentPlayer.otherPlayer))
                   .map(field.locationToMove(pieceLocation, _))
  }
}
object Bishop extends Piece(3) {
  override def targetsOn(field: FieldWrapper, currentPlayer: Player,
                         pieceLocation: Location): Iterator[ChessMove] = {
    field.pathsFrom(pieceLocation, false, true, true, currentPlayer)
         .map(field.locationToMove(pieceLocation, _))
  }

}
object Tower extends Piece(5) {
  override def targetsOn(field: FieldWrapper, currentPlayer: Player,
                         pieceLocation: Location): Iterator[ChessMove] = {
    field.pathsFrom(pieceLocation, true, false, true, currentPlayer)
         .map(field.locationToMove(pieceLocation, _))
  }

}
object Queen extends Piece(10) {
  override def targetsOn(field: FieldWrapper, currentPlayer: Player,
                         pieceLocation: Location): Iterator[ChessMove] = {
    field.pathsFrom(pieceLocation, true, true, true, currentPlayer)
         .map(field.locationToMove(pieceLocation, _))
  }
}
object King extends Piece(9999) {
  override def targetsOn(field: FieldWrapper, currentPlayer: Player,
                         pieceLocation: Location): Iterator[ChessMove] = {
    field.pathsFrom(pieceLocation, true, true, false, currentPlayer)
         .map(field.locationToMove(pieceLocation, _))
  }
}

sealed abstract case class Player(num: Int) {
  def otherPlayer: Player
}
object White extends Player(1) {
  override def otherPlayer: Player = Black
  override def toString = "White"
}
object Black extends Player(2) {
  override def otherPlayer: Player = White
  override def toString = "Black"
}

case class Location(x: Int, y: Int) {
  def print = s"$x/$y"

  def moved(xShift: Int, yShift: Int): Location = {
    Location(x + xShift, y + yShift)
  }

}
case class ChessMove(from: Location, to: Location, taken: Option[PieceOfPlayer]) extends Move
case class PieceOfPlayer(piece: Piece, owner: Player) {
  def print = s"${piece.pieceName} of $owner"

  def allMovesOnBoard(field: FieldWrapper, pieceLocation: Location): Iterator[ChessMove] = {
    piece.targetsOn(field, owner, pieceLocation).iterator
  }
}

class ChessBoard extends MutableBoard[ChessMove] {
  def attackableFieldsSum(player: Player) = {
    validMovesOf(player).size
  }

  def isTurnOfMaximizingPlayer = currentPlayer == White

  def pieceAt(x: Int, y: Int) = field(x)(y)

  private def allLocations = {
    (0 to 7).iterator.flatMap { x =>
      (0 to 7).map { y => Location(x, y) }
    }
  }

  def activePiecesSum(viewpoint: Player): Int = {
    allLocations.map { where =>
      field(where.x)(where.y) match {
        case Some(pieceAndOwner) =>
          if (pieceAndOwner.owner == viewpoint) {
            pieceAndOwner.piece.value
          } else {
            0
          }
        case None => 0
      }
    }.sum
  }

  private var currentPlayer: Player = White
  private var winner                = Option.empty[Player]

  def player = currentPlayer

  private val field = {
    val wip = Array.fill[Option[PieceOfPlayer]](8, 8)(None)
    wip(0)(0) = Some(PieceOfPlayer(Tower, White))
    wip(1)(0) = Some(PieceOfPlayer(Horse, White))
    wip(2)(0) = Some(PieceOfPlayer(Bishop, White))
    wip(3)(0) = Some(PieceOfPlayer(King, White))
    wip(4)(0) = Some(PieceOfPlayer(Queen, White))
    wip(5)(0) = Some(PieceOfPlayer(Bishop, White))
    wip(6)(0) = Some(PieceOfPlayer(Horse, White))
    wip(7)(0) = Some(PieceOfPlayer(Tower, White))
    wip(0)(1) = Some(PieceOfPlayer(Pawn, White))
    wip(1)(1) = Some(PieceOfPlayer(Pawn, White))
    wip(2)(1) = Some(PieceOfPlayer(Pawn, White))
    wip(3)(1) = Some(PieceOfPlayer(Pawn, White))
    wip(4)(1) = Some(PieceOfPlayer(Pawn, White))
    wip(5)(1) = Some(PieceOfPlayer(Pawn, White))
    wip(6)(1) = Some(PieceOfPlayer(Pawn, White))
    wip(7)(1) = Some(PieceOfPlayer(Pawn, White))

    wip(0)(6) = Some(PieceOfPlayer(Pawn, Black))
    wip(1)(6) = Some(PieceOfPlayer(Pawn, Black))
    wip(2)(6) = Some(PieceOfPlayer(Pawn, Black))
    wip(3)(6) = Some(PieceOfPlayer(Pawn, Black))
    wip(4)(6) = Some(PieceOfPlayer(Pawn, Black))
    wip(5)(6) = Some(PieceOfPlayer(Pawn, Black))
    wip(6)(6) = Some(PieceOfPlayer(Pawn, Black))
    wip(7)(6) = Some(PieceOfPlayer(Pawn, Black))
    wip(0)(7) = Some(PieceOfPlayer(Tower, Black))
    wip(1)(7) = Some(PieceOfPlayer(Horse, Black))
    wip(2)(7) = Some(PieceOfPlayer(Bishop, Black))
    wip(3)(7) = Some(PieceOfPlayer(King, Black))
    wip(4)(7) = Some(PieceOfPlayer(Queen, Black))
    wip(5)(7) = Some(PieceOfPlayer(Bishop, Black))
    wip(6)(7) = Some(PieceOfPlayer(Horse, Black))
    wip(7)(7) = Some(PieceOfPlayer(Tower, Black))
    wip
  }

  private val wrappedField = new FieldWrapper(field)

  def checkWinner(): Unit = {
    val kings = allLocations.flatMap(e => pieceAt(e.x, e.y).filter(_.piece == King)).toList
    kings.size match {
      case 2 => winner = None
      case 1 => winner = Some(kings.head.owner)
      case 0 => throw new IllegalStateException()
    }
  }

  override def applied(move: ChessMove): Unit = {
    assert(field(move.to.x)(move.to.y) == move.taken)
    field(move.to.x)(move.to.y) = field(move.from.x)(move.from.y)
    field(move.from.x)(move.from.y) = None
    flipPlayer()
    checkWinner()
  }

  override def undo(move: ChessMove): Unit = {
    field(move.from.x)(move.from.y) = field(move.to.x)(move.to.y)
    field(move.to.x)(move.to.y) = move.taken
    flipPlayer()
    checkWinner()
  }

  private def flipPlayer(): Unit = {
    currentPlayer = currentPlayer.otherPlayer
  }

  private def validMovesOf(player: Player) = {
    allLocations.flatMap { loc =>
      field(loc.x)(loc.y).filter(_.owner == player).iterator.flatMap { piece =>
        piece.allMovesOnBoard(wrappedField, loc)
      }
    }
  }

  override def validMoves: Iterator[ChessMove] = {
    validMovesOf(currentPlayer).toArray.iterator
  }

  override def boardState: BoardState = {
    if (winner.isDefined) {
      WinnerDetermined
    } else {
      OutcomeNotDetermined
    }
  }
}

object ChessRating extends BoardRating[ChessMove, ChessBoard] {
  override def rateLeaf(situation: ChessBoard): Int = {
    situation.activePiecesSum(White) * 100 -
    situation.activePiecesSum(Black) * 100 +
    //situation.attackableFieldsSum(White) -
    //situation.attackableFieldsSum(Black) +
    0
  }
}

object ChessPrinter extends BoardPrinter[ChessMove, ChessBoard] {

  override def printMove(move: ChessMove, board: ChessBoard): String = {
    val piece = board.pieceAt(move.from.x, move.from.y).get
    val eaten = board.pieceAt(move.to.x, move.to.y)
    s"${piece.piece.pieceName} of ${piece.owner} from ${move.from.print} to ${move.to.print}, taking ${eaten.map(_.print).getOrElse("nothing")}"
  }

  override def printBoard(board: ChessBoard): String = {
    (0 to 7).map { y =>
      (0 to 7).map { x =>
        board.pieceAt(x, y) match {
          case Some(pieceAndOwner) =>
            val name = pieceAndOwner.piece.pieceName
            pieceAndOwner.owner match {
              case White => name.toUpperCase.padTo(8, ' ')
              case Black => name.toLowerCase.padTo(8, ' ')
            }
          case None => "        "
        }
      }.mkString
    }.mkString("\n")
  }
}

object ChessBoard {
  def main(args: Array[String]): Unit = {
    val ctx = new GameContext[ChessMove, ChessBoard](
      new ChessBoard,
      4,
      None,
      ChessRating,
      ChessPrinter,
      alphaBetaPruning = true
    )
    AutoPlay.playTwoPlayerGame(ctx)
  }
}