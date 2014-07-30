package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */

/**
 *
 * @param impendingResult
 * @param bestMoveOption best move for the current player - option, it does not exist for final states
 * @param movesLeft how many outgoing moves are left to make sure the state is losing
 */
@SerialVersionUID(1L)
case class StateStats(val impendingResult: ImpendingResult, val bestMoveOption: Option[Move], val movesLeft: Int) extends Serializable{
  require(movesLeft >= 0)

  /**
   * checks if the state is already analyzed (enough) to determine its result. That occurs either when a winning move is
   * known or all possible moves have been recognized as losing.
   * @return a Boolean signifying if the state is already recognized as winning or losing
   */
  def isSolved: Boolean = {
    if(impendingResult.result == Win)
      true
    else
      movesLeft <= 0
  }


  /*
   * the two functions below say if the
   */
  def isWon = impendingResult.result == Win
  def isProbablyLost = impendingResult.result == Loss

  def updated(curState: State, undo: UndoMove, successorStateStat: StateStats): StateStats = {
    val possibleNewResult: ImpendingResult = successorStateStat.impendingResult.forParent
    val (newResult, newBestMoveOption) = if(possibleNewResult.isBetterThan(impendingResult))
      (possibleNewResult, Some(undo.move))
    else
      (impendingResult, bestMoveOption)
    val newMovesLeft = movesLeft - 1
    StateStats(newResult, newBestMoveOption, newMovesLeft)
  }
}

object StateStatsHelper {
  def finalStats = StateStats(finalResult, None, 0)

  val finalResult = ImpendingResult(Loss, 0)

  def stateStatsOptionUpdated(rules: Rules)(sso: Option[StateStats]) = sso match {
    case Some(ss) => ss.updated _
    case None => {(curState: State, undo: UndoMove, successorStateStat: StateStats) =>
      val impendingResult = successorStateStat.impendingResult.forParent
      val bestMove = undo.move
      val movesLeft = rules.legalMoveCount(curState.beforeUndo(undo)) - 1
      StateStats(impendingResult, Some(bestMove), movesLeft)
    }
  }
}

