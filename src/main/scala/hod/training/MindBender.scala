package hod.training

import scala.math.Ordering.BooleanOrdering

object MindBender {
  case class Implication(
                          ifTrue: Map[String, Boolean] => Map[String, Boolean],
                          requires: Set[String]  =Set.empty,
                            ifFalse: Map[String, Boolean] => Map[String, Boolean] = _ => Map.empty
                        )
  case class Statement(id: String, implications: List[Implication])

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
        List(
          Implication(_ => Map("D" -> false), ifFalse = _ => Map("D" -> true,"C" -> true, "B" -> true)),
          Implication(_ => Map("C" -> false),ifFalse = _ => Map("D" -> true,"C" -> true, "B" -> true)),
          Implication(_ => Map("B" -> false),ifFalse = _ => Map("D" -> true,"C" -> true, "B" -> true)),
        )
      ),
      Statement(
        "B",
        List(
          Implication(
            in => if (in("D")) Map("C" -> true) else Map.empty,
            Set("D"),
              ifFalse = in => if (in("D")) Map("C" -> false) else Map.empty
          )
        )
      ),
      Statement(
        "C",
        List(
          Implication(
            in =>
              if (in("E") && in("A")) Map("F" -> true) else Map("F" -> false),
            Set("E", "A"),
            ifFalse = in => if (in("E") && in("A")) Map("F" -> false) else Map("F" -> true)
          )
        )
      ),
      Statement(
        "D",
        List(
          Implication(
            in =>
              if (in("E") && in("B")) Map("A" -> true)              else Map("A" -> false),
            Set("E", "B"),
            in =>
              if (in("E") && in("B")) Map("A" -> false)              else Map("A" -> true),

          )
        )
      ),
      Statement(
        "E",
        List(
          Implication(
            in =>
              if (!in("B")) Map("F" -> true)             else Map("F" -> false),
            Set("B"),
            in =>
              if (!in("B")) Map("F" -> false)             else Map("F" -> true),

          )
        )
      ),
      Statement(
        "F",
        List(
          Implication(
            in => if (in("E")) Map("B" -> true) else Map.empty,
            Set("E"),
            in => if (in("E")) Map("B" -> false) else Map.empty,
          )
        )
      )
    )

    def findSolutions(state: Map[String, Boolean],
                      remaining: List[Statement],
                      all:List[Statement]): Unit = {

      remaining.headOption match {
        case Some(nextStatement) =>
          val assumeTrue = state + ((nextStatement.id, true))
          val assumeFalse = state + ((nextStatement.id, false))
          def consistent(check:Map[String, Boolean]) = {
            val (trueOrUndefined, notTrue) = all.partition { e =>
              !state.get(e.id).contains(false)
            }

            val trueOk = {
              trueOrUndefined.forall { statement =>
                statement.implications.exists { implication =>
                  val hasAllData = implication.requires.forall(check.keySet)
                  !hasAllData || {
                    val implied = implication.ifTrue(check)
                    val contradiction = {
                      implied.exists {
                        case (id, value) =>
                          check.get(id).exists(_ != value)
                      }
                    }
                    !contradiction
                  }
                }
              }
            }
            val falseOk = {
              notTrue.forall { statement =>
                statement.implications.exists { implication =>
                  val hasAllData = implication.requires.forall(check.keySet)
                  !hasAllData || {
                    val implied = implication.ifFalse(check)
                    val contradiction = {
                      implied.exists {
                        case (id, value) =>
                          check.get(id).exists(_ != value)
                      }
                    }
                    !contradiction
                  }
                }
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
