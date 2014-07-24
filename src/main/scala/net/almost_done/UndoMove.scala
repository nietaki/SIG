package net.almost_done

/**
 * Created by nietaki on 24.07.14.
 */
sealed trait UndoMove {
  val move: Move
}

case class UndoPlay(play: Play) extends UndoMove {
  override val move = play
}

/**
 * @param draw the draw move being undone
 * @param cardCounts the card counts in the usual format - count of the cards for each rank like (0, 1, 1, 3, 2, 4)
 */
case class UndoDraw(draw: Draw, cardCounts: List[Int]) extends UndoMove {
  override val move = draw
}
