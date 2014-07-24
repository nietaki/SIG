package net.almost_done

import Implicits._
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
  lazy val tableCardCount = tableCards.sum

  /**
   * the table should never be empty
   */
  lazy val cardOnTopOfTable = tableCards.lastIndexWhere(_ > 0)

  //def highestCardOfPlayer(idx: Int) = playerCards(idx).lastIndexWhere(_ > 0)

  lazy val currentPlayerCards = playerCards(0)
  lazy val otherPlayerCards = playerCards(1)

  def index = cardSplits.foldLeft(0)((acc, cardSplit) => acc * Utils.possibleCardSplitsCount + cardSplit.ord)


  protected def possibleDraws: List[Move] = {
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

  def afterMove(move: Move): State = {
    require(possibleMoves.contains(move))
    import Implicits._
    move match {
      case Draw(count) => {
        val(seq, remainingCount) = cardSplits.foldRight((IndexedSeq[CardSplit](), count)) {case (split, (vector, remainingCount)) =>
          val (newSplit, newRemainingCount): (CardSplit, Int) = if (remainingCount > 0) {
            val diff = math.min(remainingCount, split.tableCount)
            (CardSplit(split.theirs, split.ours + diff, split.table - diff), remainingCount - diff)
          } else {
            (split.withSwitchedPlayers, 0)
          }
          (newSplit +: vector, newRemainingCount)
        }
        assert(remainingCount == 0)
        seq
      }
      case Play(rank, count) => {
        val ret: IndexedSeq[CardSplit] = cardSplits.zipWithIndex.map({case (split, curRank) =>
          if (curRank == rank) {
            CardSplit(split.theirs, split.ours - count, split.table + count)
          } else {
            split.withSwitchedPlayers
          }
        })
        ret
      }
    }
  }


def beforeMove(move: Move): State = {
    require(possibleUndoMoves.contains(move))
    import Implicits._
    move match {
      case Draw(count) => ???
      case Play(rank, count) => ???
    }
  }

  //UNDO
  protected def possibleDrawUndoMoves: Seq[UndoDraw] = {
    /*
    the player drew the cards so now he has them. They couldn't have drawn all the cards they had now, they had to have
    at least one of the cards they have now, hence the until
     */
    //TODO this could be more effective within the whole algorithm if the first undos were "sticking to" the end moves
    //(1 until otherPlayerCards.sum).map(Draw(_)).toList
    val firstVialble = cardOnTopOfTable
    val startingCounts = List.fill(6)(0)
    val endingCountsUnchecked = otherPlayerCards //they couldn't have drawn more than they have
    val endingCounts = startingCounts.take(firstVialble) ::: endingCountsUnchecked.drop(firstVialble).toList

    val combinations = Utils.combinations2(startingCounts, endingCounts)
    combinations.map(combination => UndoDraw(Draw(combination.sum), combination))
  }

  def possiblePlayUndoMoves: List[UndoPlay] = {
    val maxPlaySize = tableCardCount - 1
    val sameCardCountOnTopOfTheTable = tableCards(cardOnTopOfTable)
    (1 to math.min(maxPlaySize, sameCardCountOnTopOfTheTable)).map(Play(cardOnTopOfTable, _)).map(UndoPlay(_)).toList
  }

  def possibleUndoMoves: Seq[UndoMove] = possibleDrawUndoMoves ++ possiblePlayUndoMoves
}

