package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */

/**
 * the split of cards of a rank between the two players and the table
 */
case class CardSplit(ours: Int, theirs: Int, table: Int) {
  val counts = ours :: theirs :: table :: Nil
  require(counts.sum == 4, counts)
  require(counts.forall(_ >= 0))

  def withSwitchedPlayers: CardSplit = CardSplit(theirs, ours, table)

  def tableCount = table

  lazy val ord = Utils.cardSplitOrds(this)
}
