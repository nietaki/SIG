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
    state.possibleMoves.filter(isLegal(state)(_))
  }

  def legalMoveCount = legalMoves(_: State).length

  //TODO
  //TODO
  //TODO scalacheck tests for moves and undo moves (together)
  //TODO
  //TODO

  //UNDO
  def isUndoLegal(state: State)(move: UndoMove): Boolean = ??? //FIXME: this should all work with UndoMoves
    /*
    move match {

    case Draw(drawCount) => {
      if(drawCount == 3) {
        true
      } else if(drawCount < 3) {
        //cannot draw less than 3 cards if there's 3 or more cards to take
        state.tableCardCount == 1
      } else {
        //drawCount > 3
        settings.drawingCards == Settings.DrawingAnyNumberOfCardsGreaterOrEqualThree ||
          (settings.drawingCards == Settings.DrawingThreeCardsOrAll && state.tableCardCount == 1)
      }
    }
    case Play(rank, count) => {
      count match {
        case 1 => true
        case 4 => settings.playingFourFigures == Settings.PlayingFourFiguresAllowed
        case 3 => {
          rank match {
            case 0 => settings.playingThreeNines == Settings.PlayingThreeNinesAllowed
            case _ => (settings.playingThreeFigures == Settings.PlayingThreeFiguresAllowed) ||
              (settings.playingThreeFigures == Settings.PlayingThreeFiguresOnlyOnAFourth && state.tableCards(rank) == 4)
          }
        }
        case _ => false
      }

    }
  }

    */
  def legalUndoMoves(state:State) = state.possibleUndoMoves.filter(isUndoLegal(state)(_))


}
