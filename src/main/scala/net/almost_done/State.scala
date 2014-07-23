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

  /**
   * the table should never be empty
   */
  lazy val cardOnTopOfTable = tableCards.lastIndexWhere(_ > 0)

  lazy val currentPlayerCards = playerCards(0)

  def index = cardSplits.foldLeft(0)((acc, cardSplit) => acc * Utils.possibleCardSplitsCount + cardSplit.ord)


  protected def possibleDraws: List[Move] = {
    val tableCardCount = tableCards.sum
    if(tableCardCount <= 1) {
      List()
    } else {
      (1 until tableCardCount).map(Draw(_)).toList
    }
  }

  protected def possiblePlays: List[Move] = {
    val plays = for(
      rank <- (cardOnTopOfTable until 6);
      count <- (1 to currentPlayerCards(rank))
    ) yield Play(rank, count)
    plays.toList
  }

  /**
   * @return a list of possible moves. They aren't necessarily legal - that is to be determined by the Rules. But there
   *         are constraints:
   *         draws will always leave at least one card on the table
   *         plays will always consist of one card rank and the rank will be GEQ to the card on top of the table stack
   *         plays will always consist of at least one card and no more than the player has
   */
  def possibleMoves: List[Move] = possiblePlays ++ possibleDraws
}

