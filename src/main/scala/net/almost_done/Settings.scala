package net.almost_done

import net.almost_done.Settings._

/**
 * Created by nietaki on 23.07.14.
 */
object Settings {

  trait SimplyNamed {def name: String}

  sealed trait PlayingThreeNines extends SimplyNamed
  case object PlayingThreeNinesAllowed extends PlayingThreeNines {val name = "PTN1"}
  case object PlayingThreeNinesNotAllowed extends PlayingThreeNines {val name = "PTN0"}

  sealed trait PlayingFourFigures extends SimplyNamed
  case object PlayingFourFiguresAllowed extends PlayingFourFigures {val name = "PFF1"}
  case object PlayingFourFiguresNotAllowed extends PlayingFourFigures {val name = "PFF0"}

  sealed trait PlayingThreeFigures extends SimplyNamed
  case object PlayingThreeFiguresAllowed extends PlayingThreeFigures {val name = "PTF1"}
  case object PlayingThreeFiguresOnlyOnAFourth extends PlayingThreeFigures {val name = "PTF4"}
  case object PlayingThreeFiguresNotAllowed extends PlayingThreeFigures {val name = "PTF0"}

  sealed trait DrawingCards extends SimplyNamed
  case object DrawingThreeCards extends DrawingCards {val name = "DC3"}
  case object DrawingThreeCardsOrAll extends DrawingCards {val name = "DC3A"}
  case object DrawingAnyNumberOfCardsGreaterOrEqualThree extends DrawingCards {val name = "DCGT3"}

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

trait Settings extends SimplyNamed {
  val playingThreeNines: PlayingThreeNines
  val playingFourFigures: PlayingFourFigures
  val playingThreeFigures: PlayingThreeFigures
  val drawingCards: DrawingCards

  override def name = s"${playingThreeNines.name}-${playingFourFigures.name}-${playingThreeFigures.name}-${drawingCards.name}"

  override def toString = s"Settings: $playingThreeNines, $playingFourFigures, $playingThreeFigures, $drawingCards"
}
