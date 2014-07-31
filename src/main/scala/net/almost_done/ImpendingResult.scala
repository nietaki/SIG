package net.almost_done

/**
 * Created by nietaki on 29.07.14.
 */

case class ImpendingResult(winningAlready: Boolean, remainingMoveCount: Int) {
  def isBetterThan(other: ImpendingResult): Boolean = {
    (this.winningAlready, other.winningAlready) match {
      case (true, false) => true
      case (false, true) => false
      case (true, true) => this.remainingMoveCount < other.remainingMoveCount
      case (false, false) => this.remainingMoveCount > other.remainingMoveCount
    }
  }

  //this is wrong - if this isn't winning already, the parent isn't winning already as well
  //def forParent = ImpendingResult(!winningAlready, remainingMoveCount + 1)
}

