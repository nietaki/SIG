package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */

sealed trait Result
case object Win extends Result
case object Loss extends Result
case object Undecided extends Result

/**
 *
 * @param impendingResult
 * @param bestMoveOption best move for the current player - option, it does not exist for final states
 * @param movesLeft how many outgoing moves are left to make sure the state is losing
 */
@SerialVersionUID(1L)
case class StateStats(val impendingResult: ImpendingResult, val bestMoveOption: Option[Move], val movesLeft: Int) extends Serializable{
  require(movesLeft >= 0)

  def isSolved: Boolean = this.result != Undecided

  def result: Result = {
    if(impendingResult.winningAlready) {
      Win
    } else {
      if(movesLeft <= 0)
        Loss
      else
        Undecided
    }
  }

  def impendingResultForParent: ImpendingResult = {
    val newRemainingMoveCount: Int = this.impendingResult.remainingMoveCount + 1
    this.result match {
      case Win => ImpendingResult(false, newRemainingMoveCount) //I'm winning, so the parent wasn't probably winning
      case Undecided => ImpendingResult(false, newRemainingMoveCount) //nothing sure about myself, no reason to believe parent is winning
      case Loss => ImpendingResult(true, newRemainingMoveCount) //I'm surely losing, and parent could lead to it
    }
  }

  def updated(curState: State, undo: UndoMove, successorStateStat: StateStats): StateStats = {
    val possibleNewResult: ImpendingResult = successorStateStat.impendingResultForParent
    val (newResult, newBestMoveOption) = if(possibleNewResult.isBetterThan(impendingResult))
      (possibleNewResult, Some(undo.move))
    else
      (impendingResult, bestMoveOption)
    val newMovesLeft = movesLeft - 1
    StateStats(newResult, newBestMoveOption, newMovesLeft)
  }
  override def toString() = {
    s"${this.result} in ${impendingResult.remainingMoveCount} moves, $movesLeft moves left, ${bestMoveOption.fold("WAT")(_.moveDescription)} looks best"
  }
}

object StateStatsHelper {
  def finalStats = StateStats(finalResult, None, 0)

  val finalResult = ImpendingResult(false, 0)

  def stateStatsOptionUpdated(rules: Rules)(sso: Option[StateStats]) = sso match {
    case Some(ss) => ss.updated _
    case None => {(curState: State, undo: UndoMove, successorStateStat: StateStats) =>
      val impendingResult = successorStateStat.impendingResultForParent
      val bestMove = undo.move
      val movesLeft = rules.legalMoveCount(curState.beforeUndo(undo)) - 1
      StateStats(impendingResult, Some(bestMove), movesLeft)
    }
  }
}

