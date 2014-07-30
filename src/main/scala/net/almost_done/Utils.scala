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
    val playersCards = shuffled.splitAt(11)

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


def combinations(startingCounts: List[Int], endingCounts: List[Int]): IndexedSeq[List[Int]] = {
    for(
      a <- startingCounts(0) to endingCounts(0);
      b <- startingCounts(1) to endingCounts(1);
      c <- startingCounts(2) to endingCounts(2);
      d <- startingCounts(3) to endingCounts(3);
      e <- startingCounts(4) to endingCounts(4);
      f <- startingCounts(5) to endingCounts(5)
    ) yield List(a, b, c, d, e, f)
  }

  import scala.annotation.tailrec

  type SLInt = IndexedSeq[List[Int]]
  def combinations2(startingCounts: List[Int], endingCounts: List[Int] ): IndexedSeq[List[Int]] = {
    @tailrec
    def inner(acc: SLInt, startingCounts: List[Int], endingCounts: List[Int]): SLInt = {
      (startingCounts, endingCounts) match {
        case (sh :: st, eh :: et) if (sh <= eh) => {
          val newAcc = for(
            ls <- acc;
            last <- (sh to eh)
          ) yield (last :: ls)
          inner(newAcc, st, et)
        }
        case (Nil, Nil) => acc
        case _ => throw new IllegalArgumentException()
      }
    }
    inner(IndexedSeq(List()), startingCounts.reverse, endingCounts.reverse)
  }

  /**
   * @return a list of end states. Note, those are *after* the winning move, so the "other" player has no cards
   */
  def endStates: List[State] = {
    def splitGen: List[CardSplit] = {
      for (
        table <- (0 to 4).toList
      ) yield CardSplit(4 - table, 0, table)
    }
    def zeroSplitGen: List[CardSplit] = {
      for (
        table <- (1 to 4).toList
      ) yield CardSplit(4 - table, 0, table)
    }
    for (
      nine <- zeroSplitGen;
      ten <- splitGen;
      jack <- splitGen;
      queen <- splitGen;
      king <- splitGen;
      ace <- splitGen
    ) yield State(IndexedSeq(nine, ten, jack, queen, king, ace))
  }


}
