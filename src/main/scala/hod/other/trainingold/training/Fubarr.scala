package hod.other.trainingold.training

import scala.collection.mutable

object Fubarr {
  def main(args: Array[String]): Unit = {
        val all = mutable.HashSet.empty[Long]
    all += 5
    1 to 10 foreach { _ =>
      all.toList.foreach { e =>
        all += e+6
        //all += e*e
      }
    }
    println(all.toList.sorted)
  }
}
