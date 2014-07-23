package net.almost_done

import net.almost_done.Settings._

/**
 * Created by nietaki on 23.07.14.
 */
object Settings {
  sealed trait PlayingThreeNines
  case object PlayingThreeNinesAllowed extends PlayingThreeNines
  case object PlayingThreeNinesNotAllowed extends PlayingThreeNines

  sealed trait  PlayingFourFigures
  case object PlayingFourFiguresAllowed extends PlayingFourFigures
  case object PlayingFourFiguresNotAllowed extends PlayingFourFigures

  sealed trait PlayingThreeFigures
  case object PlayingThreeFiguresAllowed extends PlayingThreeFigures
  case object PlayingThreeFiguresOnlyOnAFourth extends PlayingThreeFigures
  case object PlayingThreeFiguresNotAllowed extends PlayingThreeFigures

  sealed trait DrawingCards
  case object DrawingThreeCards extends DrawingCards
  case object DrawingThreeCardsOrAll extends DrawingCards
  case object DrawingAnyNumberOfCardsGreaterThanThree extends DrawingCards

  /* rule sets */

  object Simplest extends Settings {
    override val playingThreeNines: PlayingThreeNines = PlayingThreeNinesNotAllowed
    override val playingFourFigures: PlayingFourFigures = PlayingFourFiguresNotAllowed
    override val playingThreeFigures: PlayingThreeFigures = PlayingThreeFiguresNotAllowed
    override val drawingCards: DrawingCards = DrawingThreeCards
  }

  object Default extends Settings {
    override val playingThreeNines: PlayingThreeNines = PlayingThreeNinesNotAllowed
    override val playingFourFigures: PlayingFourFigures = PlayingFourFiguresAllowed
    override val playingThreeFigures: PlayingThreeFigures = PlayingThreeFiguresNotAllowed
    override val drawingCards: DrawingCards = DrawingAnyNumberOfCardsGreaterThanThree
  }
}

trait Settings {
  val playingThreeNines: PlayingThreeNines
  val playingFourFigures: PlayingFourFigures
  val playingThreeFigures: PlayingThreeFigures
  val drawingCards: DrawingCards

}
