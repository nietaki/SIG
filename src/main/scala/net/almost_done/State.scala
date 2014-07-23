package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */

/**
 * the state is always devised in such a way, that the player currently moving is at index 0 in all the arrays
 * @param cardSplits an indexed sequence of how the cards of different ranks are split between players
 */
case class State(cardSplits: IndexedSeq[CardSplit]) {
  require(cardSplits.length == 6)

  /**
   * @param idx 0 is the currently moving player, 1 is the other one
   * @return a sequence of how many cards of each rank the player has
   */
  def playerCards(idx: Int): IndexedSeq[Int] = {
    require(idx >= 0)
    require(idx <= 1)
    cardSplits.map(cs => cs.counts(idx))
  }

  def tableCards = cardSplits.map(_.tableCount)

  def currentPlayerCards = playerCards(0)

  def index = cardSplits.foldLeft(0)((acc, cardSplit) => acc * Utils.possibleCardSplitsCount + cardSplit.ord)
}

