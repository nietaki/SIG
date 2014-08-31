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
  def isFinal: Boolean = (0 to 1).exists {idx => playerCards(idx).forall(_ == 0) }
  def isStarting: Boolean = tableCards.sum == 0 //no cards on the table

  def tableCards = cardSplits.map(_.tableCount)
  lazy val tableCardCount = tableCards.sum

  def topTableCards(cardCount: Int): List[Int] = {
    require(cardCount < this.tableCardCount)
    val starting: (Int, List[Int]) = (cardCount, Nil)
    val ret = tableCards.foldRight(starting) {case (curCount, (remaining, tail)) =>
      if(curCount <= remaining) {
        (remaining - curCount, curCount :: tail)
      } else {
        (0, remaining :: tail)
      }
    }
    ret._2
  }

  /**
   * the table should never be empty
   */
  lazy val cardOnTopOfTable = tableCards.lastIndexWhere(_ > 0)

  //def highestCardOfPlayer(idx: Int) = playerCards(idx).lastIndexWhere(_ > 0)

  lazy val currentPlayerCards = playerCards(0)
  lazy val otherPlayerCards = playerCards(1)

  lazy val index = cardSplits.foldLeft(0)((acc, cardSplit) => acc * Utils.possibleCardSplitsCount + cardSplit.ord)


  protected def possibleDraws: List[Move] = {
    if(tableCardCount <= 1) {
      Nil
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

  def getUndoForMove(move: Move) = move match {
    case play: Play => UndoPlay(play)
    case Draw(count) => UndoDraw(Draw(count), this.topTableCards(count))
  }

  def beforeUndo(undo: UndoMove): State = {
    import Implicits._
    undo match {
      case UndoDraw(draw, cardCounts) => {
        //the other player drew the cards, whe have to give them back to the table and switch the players
        require(draw.count == cardCounts.sum)
        val splitsAndDrawnCards: IndexedSeq[(CardSplit, Int)] = this.cardSplits.zip(cardCounts)
        val newSplits = splitsAndDrawnCards.map({case (split, cardCount) =>
            split match {
              case CardSplit(ours, theirs, table) => CardSplit(theirs - cardCount, ours, table + cardCount)
            }
        })
        newSplits
      }
      case UndoPlay(play) => {
        //the other player played the cards, we need to give them back from the table
        play match {
          case Play(rank, count) => {
            val newSplits = this.cardSplits.zipWithIndex map { case (split, index) =>
              if(rank != index) {
                //cards remain unchanged
                CardSplit(split.theirs, split.ours, split.table)
              } else {
                //giving cards back to the other player
                CardSplit(split.theirs + count, split.ours, split.table - count)
              }
            }
            newSplits
          }
          case _ => throw new IllegalArgumentException()
        }
      }
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

    val combinations = Utils.combinations2(startingCounts, endingCounts).filter(_.sum > 0)
    combinations.map(combination => UndoDraw(Draw(combination.sum), combination))
  }

  def possiblePlayUndoMoves: List[UndoPlay] = {
    val maxPlaySize = tableCardCount - 1
    val sameCardCountOnTopOfTheTable = tableCards(cardOnTopOfTable)
    (1 to math.min(maxPlaySize, sameCardCountOnTopOfTheTable)).map(Play(cardOnTopOfTable, _)).map(UndoPlay(_)).toList
  }

  def possibleUndoMoves: Seq[UndoMove] = possibleDrawUndoMoves ++ possiblePlayUndoMoves
}

