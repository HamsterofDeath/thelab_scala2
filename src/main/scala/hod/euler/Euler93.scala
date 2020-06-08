package hod.euler

object Euler93 {
  sealed trait Calculation {
    def calculate(a: BigDecimal, b: BigDecimal): BigDecimal
  }
  case object Add extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = a + b
  }
  case object Sub extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = a - b
  }
  case object Mul extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = a * b
  }
  case object Div extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = a / b
  }

  def main(args: Array[String]): Unit = {
    val calculations =
      List(Add, Sub, Mul, Div).flatMap(e => List(e, e, e))
    val comb =
      calculations.combinations(3).toList.flatMap(_.permutations).distinct

    def findAll(digits: List[Int]) = {
      val reachable = digits.permutations
        .flatMap { digitPerm =>
          comb.flatMap { calc =>
            calculateOne(digitPerm, calc)
          }
        }
        .toSet
        .toList
        .sorted
      reachable
    }

    // val pool = (1 to 9).combinations(4).map(_.toList).toList
    def pool = List(List(2, 3, 8, 9))
    def solution =
      pool
        .map { subSet =>
          val chainUntil = {
            val reachable = findAll(subSet)

            reachable
              .sliding(2, 1)
              .takeWhile {
                case List(a, b) =>
                  a + 1 == b
              }
              .size + 1
          }
          (subSet, chainUntil)
        }
        .maxBy(_._2)

    println(s"Solution: $solution")

    //  calculateOne(List(2, 3, 8, 9), List(Mul, Sub, Mul))
  }

  private def calculateOne(digits: List[Int], calc: List[Calculation]) = {
    val concreteResults = {
      val a = {
        //(((a # b) # c) # d)
        val first = calc(0)
          .calculate(digits(0), digits(1))
        val second = calc(1)
          .calculate(first, digits(2))
        val third = calc(2)
          .calculate(second, digits(3))
        third
      }

      val b = {
        //(a # b) # (c # d)
        val first = calc(0)
          .calculate(digits(0), digits(1))
        val second = calc(2)
          .calculate(digits(2), digits(3))
        val third = calc(1)
          .calculate(first, second)
        third
      }

      val c = {
        //a # (b # c # d)
        val first = calc(0)
          .calculate(digits(0), digits(1))
        val second = calc(1)
          .calculate(first, digits(2))
        val third = calc(2)
          .calculate(second, digits(3))
        third
      }

      List(a, b, c)
    }
    concreteResults.filter(_.isWhole).map(_.toInt).filter(_ > 0).map { e =>
      println(
        s"(((${digits(0)} ${calc(0)} ${digits(1)}) ${calc(1)} ${digits(2)}) ${calc(
          2
        )} ${digits(3)}) = $e"
      )
      e
    }
  }
}
