package test

import net.almost_done._
import org.specs2.ScalaCheck
import org.specs2.matcher.TraversableMatchers
import org.specs2.mutable._
import test.Generators._

import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.matcher.{Parameters, TraversableMatchers}
import org.specs2.mutable._
import spire.implicits._
import spire.math._/**
 * Created by nietaki on 04.07.14.
 */
class DeckSpec extends Specification with TraversableMatchers with ScalaCheck  {
  "Deck" should {
    "construct a correct random deck" in {
      val state = Utils.randomStartingState
      state.cardSplits.length == 6
    }
  }

  "CardSplit" should {
    "have the same ord only when it's equal to another CardSplit" ! prop {(cs1: CardSplit, cs2: CardSplit) =>
      (cs1 == cs2) == (cs1.ord == cs2.ord)
    }

    "have the same ord as their index in the possibleCardSplits array" ! Prop.forAllNoShrink(Gen.choose(0, 14)) { idx =>
      Utils.possibleCardSplits(idx).ord == idx
    }

    "have the same ord as their index in the possibleCardSplits array (the other way)" ! prop {cs:CardSplit =>
      cs == Utils.possibleCardSplits(cs.ord)
    }
  }

  "State" should {
    "have at least one 9 on the table" in prop{s: State =>
      s.tableCards(0) > 0
    }
    "have the index in the desired range" in prop{s: State=>
      s.index >= 0 && s.index < Utils.possibleStatesCount
    }
  }

}