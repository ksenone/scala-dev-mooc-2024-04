package ru.otus.module1.DataCollection1.homework

import scala.util.Random

sealed trait Ball
case class WhiteBall() extends Ball
case class BlackBall() extends Ball

object Basket {
  lazy val count: Int = 6

  lazy val balls: List[Ball] = List(
    WhiteBall(), BlackBall(), WhiteBall(),
    BlackBall(), WhiteBall(), BlackBall()
  )
}
class BallsExperiment {

  private lazy val random = new scala.util.Random

  private def getBall(exceptBall: Ball = null): Ball = {
    Option(exceptBall).fold {
      val index = random.nextInt(Basket.count)
      Basket.balls(index)
    }{ except =>
      val index = random.nextInt(Basket.count - 1)
      Basket.balls.diff(Seq(except))(index)
    }
  }

  def isFirstBlackSecondWhite: Boolean = {
    val firstBall = getBall()
    val secondBall = getBall(exceptBall = firstBall)

    (firstBall, secondBall) match {
      case (_: BlackBall, _: WhiteBall) => true
      case _ => false
    }
  }
}

/**
 * В урне 3 белых и 3 черных шара. Из урны дважды вынимают по одному шару,
 * не возвращая их обратно. Найти вероятность появления белого шара при втором испытании
 * (событие В), если при первом испытании был извлечен черный шар (событие А).
 *
 * P(B|A) = P(AB) / P(A)
 * A - при первом испытании был извлечен черный шар
 * B - появления белого шара при втором испытании
 */
object BallsTest {
  def main(args: Array[String]): Unit = {
    val countOfExperiments = 10000
    val listOfExperiments: List[BallsExperiment] = {
      for (_ <- 1 to countOfExperiments)
        yield new BallsExperiment
    }.toList

    val countOfPositiveExperiments: Float = listOfExperiments.count(_.isFirstBlackSecondWhite)

    /** P(AB) */
    val probabilityOfBlackFirstWhiteSecond: Float = {
      countOfPositiveExperiments / countOfExperiments
    }

    /** P(A) */
    val probabilityOfBlackFirst: Float = 3 / 6f

    println(probabilityOfBlackFirstWhiteSecond / probabilityOfBlackFirst)
  }
}