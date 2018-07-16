package hod.euler

import java.text.DecimalFormat
import scala.collection.mutable

object Euler352 {
  type Decimal = Double

  implicit class DecimalOps2(val d: Double) extends AnyVal {
    def pow(i: Int): Decimal = math.pow(d, i)
  }

  def lift(d: Double): Decimal = d

  case class Scenario(samples: Int, badSampleChance: Decimal) {
    def goodSampleChance: Decimal = 1 - badSampleChance
  }

  private val df = new DecimalFormat("#0.00")

  case class NextStep(expectedNegative: Decimal,
                      planIfNegative: List[SuggestedStrategy],
                      planIfPositive: List[SuggestedStrategy]) {
    def describePlan(level: Int): String = {
      val spacing = "".padTo(level, ' ')
      s"""|$spacing If positive (${df.format(expectedPositive * 100)}%), do
          |${planIfPositive.map(_.describePlan(level + 1)).mkString("\n")}
          |$spacing If negative (${df.format(expectedNegative * 100)}%), do
          |${planIfNegative.map(_.describePlan(level + 1)).mkString("\n")}""".stripMargin
    }

    val expectedPositive: Decimal = 1 - expectedNegative
    def expectedTestsIfNegative: Decimal = planIfNegative.map(_.expectedNumberOfTests).foldLeft(lift(0))(_ + _)
    def expectedTestsIfPositive: Decimal = planIfPositive.map(_.expectedNumberOfTests).foldLeft(lift(0))(_ + _)
  }

  case class SuggestedStrategy(testOnSamples: Int, nextStep: Option[NextStep], samplesTotalLeft: Int) {

    val testsIfNegative: Decimal = nextStep.map(_.expectedTestsIfNegative).getOrElse(0.0)
    val testsIfPositive: Decimal = nextStep.map(_.expectedTestsIfPositive).getOrElse(0.0)

    def describeTestPlan: String = describePlan(0)

    def describePlan(level: Int): String = {
      val spacing = "".padTo(level, ' ')
      if (testOnSamples > 0) {
        s"""|$spacing Testing $testOnSamples of $samplesTotalLeft, expecting $expectedNumberOfFollowUpTests more tests.
            |${nextStep.map(_.describePlan(level + 1)).getOrElse(s"$spacing No next step")}""".stripMargin
      } else {
        s"$spacing Nothing"
      }
    }

    def expectedNumberOfFollowUpTests: Decimal = {
      nextStep.map { e =>
        e.expectedNegative * e.expectedTestsIfNegative +
        e.expectedPositive * e.expectedTestsIfPositive
      }.getOrElse(0.0)
    }

    val expectedNumberOfTests: Decimal = {
      (if (testOnSamples > 0) 1.0 else 0.0) + expectedNumberOfFollowUpTests
    }

  }

  class Evaluate(scenario: Scenario) {
    private val noTest   = SuggestedStrategy(0, None, 0)
    private val lastTest = SuggestedStrategy(1, None, 1)

    private def probabilityOfNegativeResult(samples: Int) = {
      val probability = lift(1) - scenario.badSampleChance
      probability.pow(samples)
    }

    class Cache {
      private val cachePositive = Array.tabulate[SuggestedStrategy](scenario.samples+1)(_ => null)
      private val cacheNegative = Array.tabulate[SuggestedStrategy](scenario.samples+1)(_ => null)

      def getOrElseUpdate(samplesLeft:Int, positive:Boolean, eval: => SuggestedStrategy): SuggestedStrategy = {
        val array = if (positive) cachePositive else cacheNegative
        val stored = array(samplesLeft)
        if (stored != null) {
          stored
        } else {
          val store = eval
          array(samplesLeft) = store
          store
        }
      }
    }

    private val cache = new Cache


    def calculateChanceOfNegativeResultInPositiveGroup(testSize: Int, totalSize: Int): Decimal = {
      val allHealthy = scenario.goodSampleChance.pow(totalSize)
      val atLeastOneSick = 1 - allHealthy
      val factor = 1 / atLeastOneSick
      val probabilityOfPositiveResult = {
        val probabilityOfEvent = {
          val negativeInThisCase = testSize
          val probabilityOfCase = scenario.goodSampleChance.pow(negativeInThisCase)
          (1 - probabilityOfCase) * factor
        }
        probabilityOfEvent
      }

      1 - probabilityOfPositiveResult
    }

    private def determineSplitSizeInPositiveGroup(maxSamples: Int) = {
      // brute force
      val index = {
        val range = Range(1, maxSamples)
        range.minBy { i =>
          splitGroup(maxSamples, i).expectedNumberOfTests
        }
      }

      index
    }

    private def determineSplitSizeInUnknownGroup(maxSamples: Int) = {
      // brute force
      val index = {
        val range = Range(1, maxSamples + 1)
        range.minBy { i =>
          splitUnknownSample(maxSamples, i).expectedNumberOfTests
        }
      }

      index
    }

    private def determineFullStrategy(samplesLeft: Int, testedPositive: Boolean): SuggestedStrategy = {
      cache.getOrElseUpdate(samplesLeft, testedPositive, {
        samplesLeft match {
          case 0 => noTest // 0 samples = no test required
          case 1 if testedPositive => noTest // only 1 sample and we already know its infected
          case 1 => lastTest // 1 sample of unknown status = do 1 test
          case _ if testedPositive =>
            val sizeOfGroupToTest = {
              determineSplitSizeInPositiveGroup(samplesLeft)
            }
            splitGroup(samplesLeft, sizeOfGroupToTest)
          case _ =>
            val sizeOfGroupToTest = {
              determineSplitSizeInUnknownGroup(samplesLeft)
            }
            splitUnknownSample(samplesLeft, sizeOfGroupToTest)
        }
      })
    }

    private def splitUnknownSample(samplesLeft: Int, sizeOfGroupToTest: Int) = {
      val chanceOfNegativeResult = probabilityOfNegativeResult(sizeOfGroupToTest)
      val untested = samplesLeft - sizeOfGroupToTest
      val ifNegative = determineFullStrategy(untested, testedPositive = false)
      val ifPositive = determineFullStrategy(sizeOfGroupToTest, testedPositive = true)
      SuggestedStrategy(sizeOfGroupToTest,
        Some {
          NextStep(chanceOfNegativeResult,
            List(ifNegative),
            List(ifPositive, ifNegative)
          )
        }, samplesLeft
      )
    }

    private def splitGroup(samplesLeft: Int, sizeOfGroupToTest: Int) = {
      val chanceOfNegativeResult = {
        calculateChanceOfNegativeResultInPositiveGroup(sizeOfGroupToTest, samplesLeft)
      }
      val untested = samplesLeft - sizeOfGroupToTest
      val ifPositive = {
        val first = determineFullStrategy(sizeOfGroupToTest, testedPositive = true)
        val second = determineFullStrategy(untested, testedPositive = false)
        (first :: second :: Nil).filter(_.testOnSamples > 0)
      }
      val ifNegative = {
        determineFullStrategy(untested, testedPositive = true)
      }

      SuggestedStrategy(sizeOfGroupToTest,
        Some {
          NextStep(chanceOfNegativeResult,
            List(ifNegative),
            ifPositive)
        }, samplesLeft
      )
    }
    def determineStrategy(knownAsPositiveInitially: Boolean): SuggestedStrategy = {
      val solutionPlan = determineFullStrategy(scenario.samples, testedPositive = knownAsPositiveInitially)
      solutionPlan
    }

    def formatShort(knownAsPositiveInitially: Boolean): String = {
      val p = determineStrategy(knownAsPositiveInitially)
      s"T(${scenario.samples}, ${scenario.badSampleChance}) = ${p.expectedNumberOfTests}"
    }

  }

  def main(args: Array[String]): Unit = {

    def explain(samples: Int, p: Decimal, knownAsPositiveInitially: Boolean) = {
      val scenario = Scenario(samples, p)
      val eval = new Evaluate(scenario)
      println(eval.formatShort(false))
      eval.determineStrategy(knownAsPositiveInitially)
    }

    val solution = {
      Range.inclusive(1, 50).par.map { i =>
        val p = 0.01 * i
        explain(10000, p, knownAsPositiveInitially = false).expectedNumberOfTests
      }.sum
    }
    println(solution)
    //    explain(25,0.02, knownAsPositiveInitially = false)
    //    explain(25,0.10, knownAsPositiveInitially = false)
  }
}
