package net.almost_done

import spire.math._
import spire.implicits._
import scala.util.Random

/**
 * Created by nietaki on 23.07.14.
 */
object Utils {
  lazy val possibleCardSplits = for (
    x <- 0 to 4;
    y <- 0 to 4;
    z <- 0 to 4;
    if(x + y + z == 4)
  ) yield CardSplit(x, y, z)

  val cardSplitOrds = possibleCardSplits.zipWithIndex.toMap

  lazy val possibleCardSplitsCount = possibleCardSplits.length
  assert(possibleCardSplitsCount == 15)

  lazy val possibleStatesCount = implicitly[Integral[Int]].pow(possibleCardSplitsCount, 6)

  val allCards = (0 until 6).flatMap(x => Vector.fill(4)(x)).toVector
  assert(allCards.size == 24)

  val cardsWithoutOneNine = allCards.drop(1)

  def randomStartingState: State = {
    //the starting player has already played the 9 of hearts
    val shuffled = Random.shuffle(cardsWithoutOneNine)
    val playersCards = shuffled.splitAt(12)

    def extractCounts(cards: Vector[Int]): IndexedSeq[Int] = {
      (0 until 6).map(rank => cards.count(_ == rank))
    }
    val pl0Counts = extractCounts(playersCards._1)
    val pl1Counts = extractCounts(playersCards._2)

    val tableCounts = Vector.fill(6)(0).updated(0, 1)

    val ret = (0 until 6).map(idx => CardSplit(pl0Counts(idx), pl1Counts(idx), tableCounts(idx)))
    State(ret)
  }

  def cardRepresentation(ord: Int):String = {
    ord match {
      case 0 => "9"
      case 1 => "10"
      case 2 => "J"
      case 3 => "D"
      case 4 => "K"
      case 5 => "A"
      case _ => throw new IllegalArgumentException()
    }
  }

  def cardsRepresentation(cardCounts: Seq[Int]): String = {
    val cards = cardCounts.zipWithIndex.flatMap({case (count, card) => Seq.fill(count)(card)})
    cards.length.toString + ": " + cards.map(cardRepresentation(_)).fold("")(_ + _)
  }

}
