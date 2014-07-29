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
  var movesLeft: Option[Int] = None

  /**
   * checks if the state is already analyzed (enough) to determine its result. That occurs either when a winning move is
   * known or all possible moves have been recognized as losing.
   * @return a Boolean signifying if the state is already recognized as winning or losing
   */
  def isSolved: Boolean = {
    assert(impendingResultOption.isEmpty == bestMoveOption.isEmpty)
    if(impendingResultOption.isDefined && impendingResultOption.get.result == Win)
      true
    else
      movesLeft.fold(false)(_  <= 0)
  }

}
