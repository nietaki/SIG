package test

import java.io._

import net.almost_done._
import test.Generators._

import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.matcher.{Expectable, Matcher, Parameters, TraversableMatchers}
import org.specs2.mutable._
import spire.implicits._
import spire.math._

import scala.pickling.binary.BinaryPickle
import scala.pickling.io.TextFileOutput
import scala.util.Random

/**
 * Created by nietaki on 04.07.14.
 */
class DeckSpec extends Specification with TraversableMatchers with ScalaCheck  {
  override implicit def defaultParameters = new Parameters(minTestsOk = 1000)

  def beSameSequenceAs[T](seq: Seq[T]): Matcher[Traversable[T]] = new Matcher[Traversable[T]] {
    def apply[S <: Traversable[T]](t: Expectable[S]) = {
      val correctLength = seq.length == t.value.toSeq.length
      val sameValues = seq.zip(t.value.toSeq).forall({case (a, b) => a == b})
      result(correctLength && sameValues,
        t.value.toList.toString() + "\n  contains the same elements as\n"+ seq.toList.toString(),
        t.value.toList.toString() + "\n does not contain the same elements in same order as \n" + seq.toList.toString(),
        t)
    }
  }
  "a random starting deck" should {
    "be of the right length" in {
      (0 to 100).foreach{ i =>
        val state = Utils.randomStartingState
        state.cardSplits.length mustEqual(6)
      }
    }
    "have nothing on the table" in {
      (0 to 100).foreach{ i =>
        val state = Utils.randomStartingState
        state.tableCards should beSameSequenceAs(List(0,0,0,0,0,0))
      }
    }
    "have the first player have twelve cards" in {
      (0 to 100).foreach{ i =>
        val state = Utils.randomStartingState
        state.playerCards(0).sum should beEqualTo(12)
      }
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
  "traversable matchers" should {
    "be able to compare collections" ! prop{ls: List[Int] =>
      ls must containTheSameElementsAs(ls)
    }

    "compare collections when shuffled" ! prop {ls: List[Int] =>
      ls must containTheSameElementsAs(Random.shuffle(ls))
    }

    "compare collections in order" ! prop {ls: List[Int] =>
      ls must beSameSequenceAs(ls.toIndexedSeq)
    }

    "notice when the collection is in a different order" in {
      List(1,2,3) must beSameSequenceAs(List(3,2,1)) not
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
        case Draw(count) => {
          val draw = Draw(count)
          val cardsDrawn = s.topTableCards(count)
          s.afterMove(draw).beforeUndo(UndoDraw(draw, cardsDrawn)) mustEqual(s)
        }
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

  "All Settings" should {
    "allow to undo all the moves they allowed to do" ! prop { (state: State, settings: Settings) =>
      val r = new Rules(settings)
      r.legalMoves(state).forall{m: Move =>
        val undo = state.getUndoForMove(m)
        val newState = state.afterMove(m)
        if(!r.isUndoLegal(newState)(undo)) {
          //I'll leave this here for posterity, to show how this test helped find an otherwise difficult to find bug
          println("original state")
          println(state)
          println("move")
          println(m)
          println("undo")
          println(undo)
          println("newState")
          println(newState)
          false
        } else true
      }
    }
    "allow to redo all the moves they allowed to undo" ! prop { (state: State, settings: Settings) =>
      val r = new Rules(settings)
      r.legalUndoMoves(state).forall{undo: UndoMove =>
        val newState = state.beforeUndo(undo)
        val ret = r.isLegal(newState)(undo.move)
        if(!ret){
          println(s"Move ${undo.move} is not legal in $newState")
        }
        ret
      }
    }
  }

  "Serialization" should {
    val arr = Array.fill(Utils.possibleStatesCount / 1000 )(Some(StateStatsHelper.finalStats))
    "java serialization save and read a short stats array" in {
      val filename = "javaSerialization.tmp"
      val file: File = new File(filename)
      //~2.9 for 1/100 of the possible state count
      val out = new ObjectOutputStream(new FileOutputStream(file))
      out.writeObject(arr)
      out.close
      val in = new ObjectInputStream(new FileInputStream(new File(filename)))
      val restoredArr = in.readObject().asInstanceOf[Array[Option[StateStats]]]
      in.close
      file.delete()
      //println(restoredArr.toList)
      true
    }

    "scala picking serialization save and read stats array" in pending/*{
      import scala.pickling._
      //import binary._
      import json._
      val file = new File("pickle.tmp")
      val fileOut = new TextFileOutput(file)

      arr.pickleTo(fileOut)
      //TODO check if it's correct
    }*/
    case class PersonNums(name: String, randNums: Array[Int])
    "run test from examples" in pending /*{
      import scala.pickling._
      import json._
      import scala.io.Source
      val p = PersonNums("James", (1 to 200).toArray)

      val tmpFile = File.createTempFile("pickling", "fileoutput")
      val fileOut = new TextFileOutput(tmpFile)

      p.pickleTo(fileOut)
      fileOut.close()

      val fileContents = Source.fromFile(tmpFile).getLines.mkString("\n")

      fileContents mustEqual(p.pickle.value)
    }*/
  }

}