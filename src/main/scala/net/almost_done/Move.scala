package net.almost_done

/**
 * Created by nietaki on 23.07.14.
 */
sealed trait Move {
  def moveDescription: String
}

/**
 * the move where the current player plays one or more cards of the same rank
 * @param rank the card rank
 * @param count how many cards are played
 */
case class Play(rank: Int, count: Int) extends Move {
  override def moveDescription = s"play $count ${Utils.cardRepresentation(rank)}"
}

/**
 * the move where the current player draws one or more cards from the stack
 * @param count how many cards are drawed
 */
case class Draw(count: Int) extends Move {
  override def moveDescription: String = s"draw $count cards"
}
