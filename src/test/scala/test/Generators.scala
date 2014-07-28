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
      y <- Gen.choose(0, 4-x)
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

  val stateAndLessThanTableCardCount: Gen[(State, Int)] = {
    for(
      s <- correctState;
      count <- Gen.choose(0, s.tableCardCount - 1)
    ) yield (s, count)
  }

  val settings: Gen[Settings] = {
    import Settings._
    for(
      ptn <- Gen.oneOf(PlayingThreeNinesAllowed, PlayingThreeNinesNotAllowed);
      pff <- Gen.oneOf(PlayingFourFiguresAllowed, PlayingFourFiguresNotAllowed);
      ptf <- Gen.oneOf(PlayingThreeFiguresAllowed, PlayingThreeFiguresOnlyOnAFourth, PlayingThreeFiguresNotAllowed);
      dc <- Gen.oneOf(DrawingThreeCards, DrawingThreeCardsOrAll, DrawingAnyNumberOfCardsGreaterOrEqualThree)
    ) yield Settings(ptn, pff, ptf, dc)
  }

  implicit val arbitrarySettings: Arbitrary[Settings] = Arbitrary(settings)

}
