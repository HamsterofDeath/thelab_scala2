package hod.euler

import java.io.File
import java.util
import scala.collection._
import scala.collection.parallel.CollectionConverters._

/**
 * Developed with pleasure
 * User: hamsterofdeath
 * Date: 05.10.12
 * Time: 08:48
 */
object Euler96 {

  def main(args: Array[String]) {
    type MyBitSet = RawLongBits
    trait Bits[SELF <: Bits[SELF]] extends Foreach[Int] {
      def on(i: Int)
      def off(i: Int)
      def clear()
      def |(bits: SELF)
      def &(bits: SELF)
      def bitCount: Int
      def foreach[U](f: Int => U)
    }

    class UtilBits(private var bits: util.BitSet) extends Bits[UtilBits] {
      def foreach[U](f: Int => U) {
        var i = bits.nextSetBit(1)
        while (i >= 1) {
          f(i)
          i += 1
          i = bits.nextSetBit(i)
        }
      }
      def on(i: Int) {bits.set(i)}
      def off(i: Int) {bits.clear(i)}
      def clear() {bits.clear()}
      def |(other: UtilBits) {bits.or(other.bits)}
      def &(other: UtilBits) {bits.and(other.bits)}
      def bitCount: Int = bits.cardinality
    }

    class RawLongBits(private var bits: Long) extends Bits[RawLongBits] {
      def foreach[U](f: Int => U) {
        var i = 1
        while (i <= 9) {
          if ((bits & (1 << i)) == (1 << i)) f(i)
          i += 1
        }
      }
      def on(i: Int) {bits |= (1 << i)}
      def off(i: Int) {bits &= ~(1 << i)}
      def clear() {bits = 0}
      def |(other: RawLongBits) {bits |= other.bits}
      def &(other: RawLongBits) {bits &= other.bits}
      def bitCount: Int = java.lang.Long.bitCount(bits)
    }

    class ScalaBits(private var bits: mutable.BitSet) extends Bits[ScalaBits] {
      def foreach[U](f: Int => U) {bits.foreach(f)}
      def on(i: Int) {bits.add(i)}
      def off(i: Int) {bits.remove(i)}
      def clear() {bits.clear()}
      def |(other: ScalaBits) {bits = bits | other.bits}
      def &(other: ScalaBits) {bits = bits & other.bits}
      def bitCount: Int = bits.size
    }

    val nineBits = 2 | 4 | 8 | 16 | 32 | 64 | 128 | 256 | 512

    def nineBitSet: MyBitSet = new RawLongBits(nineBits)

    def myBitSet: MyBitSet = new RawLongBits(0)

    class Cell(var number: Int, var row: Row, var col: Col, var block: Block, val indexInArray: Int) {
      private var freeNumbersInvalid  = true
      private val freeNumbersVolatile = myBitSet

      def invalidateFreeNumbers() {
        freeNumbersInvalid = true
      }

      def isNumberSet: Boolean = number > 0

      def this(initialNumber: Int, indexInArray: Int) {
        this(initialNumber, null, null, null, indexInArray)
      }

      def setNumber(newNumber: Int) {
        assert(number == 0)
        number = newNumber
        myReferences.foreach(_.notifyNumberSet(number))
      }

      def unsetNumber() {
        myReferences.foreach(_.notifyNumberUnset(number))
        number = 0
      }

      override def toString: String = number + "@" + (1 + indexInArray % 9) + "/" + (1 + indexInArray / 9)

      def freeNumbers: MyBitSet = {
        if (freeNumbersInvalid) {
          freeNumbersInvalid = false
          freeNumbersVolatile.clear()
          freeNumbersVolatile | row.free
          freeNumbersVolatile & col.free
          freeNumbersVolatile & block.free
        }
        freeNumbersVolatile
      }

      def myReferences: Iterable[NineCells] = new Iterable[NineCells] {
        override def iterator: Iterator[NineCells] = Iterator(row, col, block)
      }
    }
    abstract class NineCells(val cells: Array[Cell], val free: MyBitSet = nineBitSet) {
      def notifyNumberUnset(number: Int) {
        free.on(number)
        cells.foreach(_.invalidateFreeNumbers())
      }

      def notifyNumberSet(number: Int) {
        free.off(number)
        cells.foreach(_.invalidateFreeNumbers())
      }

      def initializeFreeNumbers() {
        cells.foreach(e => free.off(e.number))
      }

      def initReferences()
    }
    class Row(row: Array[Cell]) extends NineCells(row) {
      def initReferences() {
        row.foreach(_.row = Row.this)
      }
    }
    class Block(block: Array[Cell]) extends NineCells(block) {
      def initReferences() {
        block.foreach(_.block = Block.this)
      }
    }
    class Col(col: Array[Cell]) extends NineCells(col) {
      def initReferences() {
        col.foreach(_.col = Col.this)
      }
    }
    class Sudoku(val cells: Array[Cell], val rows: Array[Row], val cols: Array[Col], val blocks: Array[Block]) {
      private var openCellCount = 81

      def checkSum: Int = cells(0).number * 100 + cells(1).number * 10 + cells(2).number

      def pretty: String = rows.map(_.cells.map(_.number).mkString).mkString("\n")

      def isSolved: Boolean = openCellCount == 0

      def allNines = new Foreach[NineCells] {
        def foreach[U](f: NineCells => U) {
          rows.foreach(f)
          cols.foreach(f)
          blocks.foreach(f)
        }
      }

      def setNumber(index: Int, number: Int) {
        cells(index).setNumber(number)
        openCellCount -= 1
      }

      def unsetNumber(index: Int) {
        cells(index).unsetNumber()
        openCellCount += 1
      }

      def initialScan() {
        allNines.foreach(_.initializeFreeNumbers())
        openCellCount = 81 - cells.count(_.isNumberSet)
      }
    }
    def xy2Index(col: Int, row: Int) = col + row * 9

    def block2xy(block: Int) = ((block % 3) * 3) -> ((block / 3) * 3)

    def block2Area(block: Int): Foreach[Int] = {
      val upperLeft = block2xy(block)
      new Foreach[Int] {
        def foreach[U](f: Int => U) {
          for (row <- 0 to 2; col <- 0 to 2) {
            f(xy2Index(upperLeft._1 + col, upperLeft._2 + row))
          }
        }
      }
    }

    def parse(raw: String) = {
      val rowsConcatenated = raw.zipWithIndex.map(char => new Cell(char._1.getNumericValue, char._2)).toArray
      val rows = rowsConcatenated.grouped(9).map(nine => new Row(nine)).toArray
      val cols = (0 to 8).map(col => new Col(rowsConcatenated.drop(col).sliding(1, 9).flatten.toArray)).toArray
      val blocks = {
        val cellGroups = for (block <- 0 to 8) yield block2Area(block)
                                                     .map(indices => rowsConcatenated(indices))
        for (nine <- cellGroups) yield new Block(nine.iterator.toArray)
      }
      val sudoku = new Sudoku(rowsConcatenated, rows, cols, blocks.toArray)
      sudoku.allNines.foreach(_.initReferences())
      sudoku.initialScan()
      sudoku
    }

    def solve(sudoku: Sudoku) {
      if (!sudoku.isSolved) {
        val myMove = {
          def nextMove: Cell = {
            sudoku.cells.iterator.filterNot(_.isNumberSet).minBy(e => {
              val size = e.freeNumbers.bitCount
              if (size == 1) return e
              if (size == 0) 10 else size
            })
          }

          nextMove
        }
        myMove.freeNumbers.foreach(num => {
          //println("setting number to " + num + " at " + myMove)
          sudoku.setNumber(myMove.indexInArray, num)
          //println(sudoku.pretty)
          if (sudoku.isSolved) {
            return
          } else {
            solve(sudoku)
            if (!sudoku.isSolved) {
              //println("resetting " + myMove)
              sudoku.unsetNumber(myMove.indexInArray)
            } else {
              return
            }
          }
        })
      }
    }

    val sum = {
      val groupedToFields = {
        new File("resource/sudoku.txt")
          .slurp
          .sliding(10, 10)
          .map { tenLines =>
            tenLines.drop(1).mkString
          }
      }
      val toSolve = groupedToFields.toArray.par.map(parse)
      toSolve.foreach(solve)
      val checks = toSolve.map(_.checkSum)
      val solution = checks.sum
      solution
    }
    println(sum)
  }
}