package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */

/**
 * the split of cards of a rank between the two players and the table
 */
case class CardSplit(counts: List[Int]) {
  require(counts.length == 3)
  require(counts.sum == 4, counts)
  require(counts.forall(_ >= 0))

  def withSwitchedPlayers: CardSplit = counts match {
    case a :: b :: c :: Nil => CardSplit(b :: a :: c :: Nil)
    case _ => throw new IllegalArgumentException()
  }

  def tableCount = counts(2)

  lazy val ord = Utils.cardSplitOrds(this)


}
