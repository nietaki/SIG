package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */
class Rules(val settings: Settings) {

  /**
   * checks if the move suggested by the state is legal in in these rules. It assumes the guarantees listed in
   * State.possibleMoves, This just checks the Settings related options
   * @param state the state in which the move would be performed
   * @param move the verified move
   * @return
   */
  def isLegal(state: State)(move: Move): Boolean = move match {
    case Draw(drawCount) => {
      val availableToDraw = state.tableCardCount - 1
      if(availableToDraw <= 3) {
        availableToDraw == drawCount
      } else {
        settings.drawingCards match {
          case Settings.DrawingThreeCards => drawCount == 3
          case Settings.DrawingThreeCardsOrAll => (drawCount == 3) || (drawCount == availableToDraw)
          case Settings.DrawingAnyNumberOfCardsGreaterOrEqualThree => (drawCount >= 3) && (drawCount <= availableToDraw)
          case _ => throw new IllegalArgumentException() //no other setting for drawing cards available
        }
      }
    }
    case Play(rank, count) => {
      count match {
        case 1 => true
        case 4 => settings.playingFourFigures == Settings.PlayingFourFiguresAllowed
        case 3 => {
          if (rank == 0) //"0" is a nine
            settings.playingThreeNines == Settings.PlayingThreeNinesAllowed
          else {
            val normal = settings.playingThreeFigures == Settings.PlayingThreeFiguresAllowed
            val onFourth = (settings.playingThreeFigures == Settings.PlayingThreeFiguresOnlyOnAFourth) &&
              state.cardOnTopOfTable == rank

            normal || onFourth
          }
        }
        case _ => false //all other card counts are invalid
      }
    }
    case _ => throw new IllegalArgumentException() //no other move kinds
  }

  def legalMoves(state: State): List[Move] = {
    val stateVerifier = isLegal(state) _
    state.possibleMoves.filter(stateVerifier(_))
  }
}
