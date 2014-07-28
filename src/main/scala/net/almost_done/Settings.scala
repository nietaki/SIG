package net.almost_done

import net.almost_done.Settings._

/**
 * Created by nietaki on 23.07.14.
 */
object Settings {
  sealed trait PlayingThreeNines
  case object PlayingThreeNinesAllowed extends PlayingThreeNines
  case object PlayingThreeNinesNotAllowed extends PlayingThreeNines

  sealed trait PlayingFourFigures
  case object PlayingFourFiguresAllowed extends PlayingFourFigures
  case object PlayingFourFiguresNotAllowed extends PlayingFourFigures

  sealed trait PlayingThreeFigures
  case object PlayingThreeFiguresAllowed extends PlayingThreeFigures
  case object PlayingThreeFiguresOnlyOnAFourth extends PlayingThreeFigures
  case object PlayingThreeFiguresNotAllowed extends PlayingThreeFigures

  sealed trait DrawingCards
  case object DrawingThreeCards extends DrawingCards
  case object DrawingThreeCardsOrAll extends DrawingCards
  case object DrawingAnyNumberOfCardsGreaterOrEqualThree extends DrawingCards

  def apply(ptn: PlayingThreeNines, pff: PlayingFourFigures, ptf: PlayingThreeFigures, dc: DrawingCards): Settings = {
    new Settings {
      override val playingThreeNines: PlayingThreeNines = ptn
      override val playingFourFigures: PlayingFourFigures = pff
      override val drawingCards: DrawingCards = dc
      override val playingThreeFigures: PlayingThreeFigures = ptf
    }
  }

  /* rule sets */
  object Simplest extends Settings {
    override val playingThreeNines: PlayingThreeNines = PlayingThreeNinesNotAllowed
    override val playingFourFigures: PlayingFourFigures = PlayingFourFiguresNotAllowed
    override val playingThreeFigures: PlayingThreeFigures = PlayingThreeFiguresNotAllowed
    override val drawingCards: DrawingCards = DrawingThreeCards
  }

  object Default extends Settings {
    override val playingThreeNines: PlayingThreeNines = PlayingThreeNinesAllowed
    override val playingFourFigures: PlayingFourFigures = PlayingFourFiguresAllowed
    override val playingThreeFigures: PlayingThreeFigures = PlayingThreeFiguresNotAllowed
    override val drawingCards: DrawingCards = DrawingThreeCards
  }

  object Crazy extends Settings {
    override val playingThreeNines: PlayingThreeNines = PlayingThreeNinesAllowed
    override val playingFourFigures: PlayingFourFigures = PlayingFourFiguresAllowed
    override val playingThreeFigures: PlayingThreeFigures = PlayingThreeFiguresAllowed
    override val drawingCards: DrawingCards = DrawingAnyNumberOfCardsGreaterOrEqualThree
  }

}

trait Settings {
  val playingThreeNines: PlayingThreeNines
  val playingFourFigures: PlayingFourFigures
  val playingThreeFigures: PlayingThreeFigures
  val drawingCards: DrawingCards

  override def toString = s"Settings: $playingThreeNines, $playingFourFigures, $playingThreeFigures, $drawingCards"
}
