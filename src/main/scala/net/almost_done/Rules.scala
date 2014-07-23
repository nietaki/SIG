package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */
class Rules(settings: Settings) {
  def isLegal(state: State)(move: Move): Boolean = ???


  def legalMoves(state: State): List[Move] = {
    state.possibleMoves.filter(isLegal(state)(_))
  }
}
