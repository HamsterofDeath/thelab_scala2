package hod.trainingold.training

import scala.math.Ordering.BooleanOrdering

object MindBender {
  case class Implication(isTrue: Map[String, Boolean] => Boolean,
                         requiredForCheck: Set[String]) {
    def isFalse(in: Map[String, Boolean]) = !isTrue(in)
  }
  case class Statement(id: String, implication: Implication)

  def main(args: Array[String]): Unit = {
    //Wir betrachten die folgenden Aussagen
    // (A)-(F)
    // (A)   Mindestens eines von (D), (B) und (C) ist falsch
    // (B)   Wenn (D) wahr ist, dann ist auch (C) wahr
    // (C)   (F) ist genau dann wahr, wenn (E) und (A) beide wahr sind
    // (D)   (A) ist genau dann wahr, wenn (B) und (E) beide wahr sind
    // (E)   (F) ist genau dann wahr, wenn (B) falsch ist(F)
    // (F) Wenn (E) wahr ist, dann ist auch (B) wahr

    val statements = List(
      Statement(
        "A",
        Implication(
          isTrue = in => !in("D") || !in("C") || !in("B"),
          Set("D", "C", "B")
        )
      ),
      Statement(
        "B",
        Implication(in => if (in("D")) in("C") else true, Set("D", "C"))
      ),
      Statement(
        "C",
        Implication(in => (in("E") && in("A")) == in("F"), Set("E", "A", "F"))
      ),
      Statement(
        "D",
        Implication(in => (in("E") && in("B")) == in("A"), Set("E", "B", "A"))
      ),
      Statement("E", Implication(in => in("B") != in("F"), Set("B", "F"))),
      Statement(
        "F",
        Implication(in => if (in("E")) in("B") else true, Set("E", "B"))
      )
    )

    def findSolutions(state: Map[String, Boolean],
                      remaining: List[Statement],
                      all: List[Statement]): Unit = {

      remaining.headOption match {
        case Some(nextStatement) =>
          val assumeTrue = state + ((nextStatement.id, true))
          val assumeFalse = state + ((nextStatement.id, false))
          def consistent(check: Map[String, Boolean]) = {
            val (trueOrUndefined, notTrue) = all.partition { e =>
              !state.get(e.id).contains(false)
            }

            val trueOk = {
              trueOrUndefined.forall { statement =>
                val hasAllData =
                  statement.implication.requiredForCheck.forall(check.keySet)
                !hasAllData || statement.implication.isTrue(check)
              }
            }
            val falseOk = {
              notTrue.forall { statement =>
                val hasAllData =
                  statement.implication.requiredForCheck.forall(check.keySet)
                !hasAllData || statement.implication.isFalse(check)
              }
            }

            trueOk && falseOk
          }
          if (consistent(assumeTrue)) {
            findSolutions(assumeTrue, remaining.tail, all)
          }
          if (consistent(assumeFalse)) {
            findSolutions(assumeFalse, remaining.tail, all)
          }

        case None => println(state.toList.sortBy(_._1).mkString(" & "))
      }
    }

    def solve(statements: List[Statement]): Unit =
      findSolutions(Map.empty, statements, statements)

    solve(statements)
  }
}
