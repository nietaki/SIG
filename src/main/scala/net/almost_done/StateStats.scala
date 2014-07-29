package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */
class StateStats {
  /**
   * best move for the current player
   */
  var impendingResultOption: Option[ImpendingResult] = None
  var bestMoveOption: Option[Move] = None

  /**
   * how many moves are left to make sure the state is losing
   */
  protected var movesLeft: Option[Int] = None

  /**
   * determines if the stats hasn't been visited yet - the remaining children count hasn't been checked yet and set
   * to the moves left, same with the impendingResult
   * @return
   */
  def isUntouched = movesLeft.isEmpty

  /**
   * checks if the state is already analyzed (enough) to determine its result. That occurs either when a winning move is
   * known or all possible moves have been recognized as losing.
   * @return a Boolean signifying if the state is already recognized as winning or losing
   */
  def isSolved: Boolean = {
    if(impendingResultOption.isDefined && impendingResultOption.get.result == Win)
      true
    else
      movesLeft.fold(false)(_  <= 0)
  }

  /**
   * updates this StateStat based on the information of the successor
   * @param successorStateStat the successor (isn't verified)
   */
  def updateWithCurStateUndoAndSuccessor(curState: State, undo: UndoMove, successorStateStat: StateStats) {
    assert(successorStateStat.impendingResultOption.isDefined)
    if(this.isUntouched) {
      //we need to initialize the values
      this.movesLeft = Some(curState.possibleMoves.length - 1) //we subtract for the successorStateStat already
      this.bestMoveOption = Some(undo.move)
      this.impendingResultOption = successorStateStat.impendingResultOption.map(_.forParent)
    } else {
      assert(this.movesLeft.isDefined)
      assert(this.movesLeft.get > 0)

    }
  }

  /*
   * the two functions below say if the
   */
  def isWon = impendingResultOption.fold(false)(_.result == Win)
  def isProbablyLost = impendingResultOption.fold(false)(_.result == Win)

}

object StateStats {
  def finalStats: StateStats = {
    val ret = new StateStats
    ret.impendingResultOption = Some(finalResult)
    ret.movesLeft = Some(0)
    ret
  }

  val finalResult = ImpendingResult(Loss, 0)
}



