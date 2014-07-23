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

  def playerCards(idx: Int): IndexedSeq[Int] = {
    require(idx >= 0)
    require(idx <= 1)
    cardSplits.map(cs => cs.counts(idx))
  }

  def currentPlayerCards = playerCards(0)

  def index = cardSplits.foldLeft(0)((acc, cardSplit) => acc * Utils.possibleCardSplitsCount + cardSplit.ord)
}

