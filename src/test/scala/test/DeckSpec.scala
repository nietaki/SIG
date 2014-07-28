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
import spire.math._

import scala.util.Random

/**
 * Created by nietaki on 04.07.14.
 */
class DeckSpec extends Specification with TraversableMatchers with ScalaCheck  {

  override implicit def defaultParameters = new Parameters(minTestsOk = 1000)

  "Deck" should {
    "construct a correct random deck" in {
      val state = Utils.randomStartingState
      state.cardSplits.length mustEqual(6)
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
    "be equal to another state IFF it contains the same splits" ! prop{st: (State, State) =>
      val (s1, s2) = st
      val elementsSame: Boolean = (s1.cardSplits.length == s2.cardSplits.length) &&
        s1.cardSplits.zip(s2.cardSplits).forall(splitTuple => splitTuple._1 == splitTuple._2)
      (s1 == s2) mustEqual elementsSame
    }
    "not have its index collide with a different State" ! prop{st: (State, State) =>
      val (s1, s2) = st
      (s1.index == s2.index) == (s1 == s2)
    }
    "have at least one 9 on the table" in prop{s: State =>
      s.tableCards(0) > 0
    }
    "have the index in the desired range" in prop{s: State =>
      s.index >= 0 && s.index < Utils.possibleStatesCount
    }
    "afterMove should produce states with other player's cards intact" ! prop{s: State =>
      val statesAfterMove = s.possibleMoves.map(s.afterMove(_))
      statesAfterMove.forall({ newState =>
        s.otherPlayerCards.zip(newState.currentPlayerCards).forall({case (a, b) => a == b})
      })
    }
    "afterMove should produce states with current player's cards changed" ! prop{s: State =>
      val statesAfterMove = s.possibleMoves.map(s.afterMove(_))
      statesAfterMove.forall({ newState =>
        s.currentPlayerCards.zip(newState.otherPlayerCards).exists({case (a, b) => a != b})
      })
    }

  }
  "test" should {
    "be able to compare collections" ! prop{ls: List[Int] =>
      ls must containTheSameElementsAs(ls)
    }

    "compare collections when shuffled" ! prop {ls: List[Int] =>
      ls must containTheSameElementsAs(Random.shuffle(ls))
    }
  }

  "State.topTableCards" should {
    "return the correct card count" ! Prop.forAllNoShrink(Generators.stateAndLessThanTableCardCount)({case (s, count) =>
      s.topTableCards(count).sum == count
    })

    "give no more cards than there are on the table" ! Prop.forAllNoShrink(Generators.stateAndLessThanTableCardCount)({ case (s, count) =>
      s.topTableCards(count).zip(s.tableCards).forall(tuple => tuple._1 <= tuple._2)
    })

    "give all the cards in all but the lowest (if that makes sense)" ! Prop.forAllNoShrink(Generators.stateAndLessThanTableCardCount)({ case (s, count) =>
      val top = s.topTableCards(count)
      val firstIndex = top.indexWhere(_ > 0)
      if(firstIndex >= 0) {
        val afterFirst = firstIndex + 1
        (afterFirst until top.length).forall(i => s.tableCards(i) == top(i))
      } else
        true
    })

  }

  "State.afterMove" should {
    "change the state" ! prop {s: State =>
      s.possibleMoves.forall {pm: Move =>
        s.afterMove(pm) != s
      }
    }
  }
  "State.beforeMove" should {
    "return to the same state after any possible play made" ! prop{s: State =>
      s.possibleMoves.foreach(_ match {
        case play: Play => s.afterMove(play).beforeUndo(UndoPlay(play)) mustEqual(s)
        case _ => () //TODO: add a test for this case
      })
    }

    "correctly redo any undone move" ! prop { s: State =>
      s.possibleUndoMoves.forall {u: UndoMove =>
        s.beforeUndo(u).afterMove(u.move) == s
      }
    }
    "change the state" ! prop { s: State =>
      s.possibleUndoMoves.forall {u: UndoMove =>
        s.beforeUndo(u) != s
      }
    }

  }

}