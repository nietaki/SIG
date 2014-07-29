package net.almost_done

/**
 * Created by nietaki on 29.07.14.
 */
sealed trait Result {}
case object Win extends Result
case object Loss extends Result

case class ImpendingResult(result: Result, remainingMoveCount: Int) {
  def isBetterThan(other: ImpendingResult): Boolean = {
    (this.result, other.result) match {
      case (Win, Loss) => true
      case (Loss, Win) => false
      case (Win, Win) => this.remainingMoveCount < other.remainingMoveCount
      case (Loss, Loss) => this.remainingMoveCount > other.remainingMoveCount
    }
  }
}

object ImpendingResultHelpers {
  /**
   * a value of ImpendingResult for all final states - after the winning move. The current player has lost after the
   * other player's last move. All makes sense,right?
   */
  val FinalResult = ImpendingResult(Loss, 0)
}
