package net.almost_done

/**
 * Created by nietaki on 29.07.14.
 */
sealed trait Result {
  def other: Result
}
case object Win extends Result {
  def other = Loss
}
case object Loss extends Result {
  def other = Win
}

case class ImpendingResult(result: Result, remainingMoveCount: Int) {
  def isBetterThan(other: ImpendingResult): Boolean = {
    (this.result, other.result) match {
      case (Win, Loss) => true
      case (Loss, Win) => false
      case (Win, Win) => this.remainingMoveCount < other.remainingMoveCount
      case (Loss, Loss) => this.remainingMoveCount > other.remainingMoveCount
    }
  }

  /**
   * @return a copy of this impending result with one more move and swapped result
   */
  def forParent = ImpendingResult(result.other, remainingMoveCount + 1)
}

