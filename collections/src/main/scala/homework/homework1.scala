package homework

import scala.util.Random

class BallsExperiment {

  val urn: List[Int] = List(1, 0, 1, 0, 1, 0)

  def isFirstBlackSecondWhite(): Boolean = {
    takeAndDelete(urn) match {
      case (0, u) => takeAndDelete(u) match {
        case (1, _) => true
        case _ => false
      }
      case _ => false
    }
  }

  def takeAndDelete(basket: List[Int]): (Int, List[Int]) = {
    val ballNum = Random.nextInt(urn.length)
    (urn(ballNum), urn.slice(0, ballNum) ::: urn.slice(ballNum + 1, urn.length))
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = List.fill(count)(new BallsExperiment)
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite())
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}