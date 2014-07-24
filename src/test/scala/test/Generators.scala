package test

import net.almost_done._
import org.scalacheck._

/**
 * Created by nietaki on 23.07.14.
 */
object Generators {

  val cardSplitGen: Gen[CardSplit] = {
    for (
      x <- Gen.choose(0, 4);
      y <- Gen.choose(0, 4-x);
      if(x + y <= 4)
    ) yield CardSplit(x, y, 4 - x - y)
  }

  implicit val arbitraryCardSplit: Arbitrary[CardSplit] = Arbitrary(cardSplitGen)

  private val correctState: Gen[State] = {
    val ls = Gen.listOfN(6, cardSplitGen)
    //could be done without the filter
    val correct = ls.filter(ls => ls(0).tableCount > 0).map(ls => State(ls.toIndexedSeq))
    correct
  }

  implicit val arbitraryState: Arbitrary[State] = Arbitrary(correctState)

}
