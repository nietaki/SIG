package test

import java.io._

import net.almost_done.Settings.Default
import net.almost_done._
import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.matcher.{Expectable, Matcher, Parameters, TraversableMatchers}
import org.specs2.mutable._
import test.Generators._

import scala.util.Random

/**
 * Created by nietaki on 04.07.14.
 */
class LongRunningSpec extends Specification with TraversableMatchers with ScalaCheck  {
  sequential
  override implicit def defaultParameters = new Parameters(minTestsOk = 1000)
  /// we're only testing with the default settings
  private val settings: Default.type = Settings.Default
  lazy val solution: GameSolver.Solution = {
    println("getting the solution (should be done once)")
    val ret = GameSolver.getSolution(settings)
    println("solution read")
    ret
  }
  val rules = new Rules(settings)
  /*
 "a random starting deck" should {
   "only lead to analyzed situations" in {
     //this gon' be ugly
     (0 to 100).foreach { i =>
       var state = Utils.randomStartingState

       (0 to 100).foreach { j =>
        solution(state.index).isDefined mustEqual(true).updateMessage(state.toString())
        if(rules.legalMoveCount(state) > 0 && !state.isFinal) {
          val moves = rules.legalMoves(state)
          val which: Int = Random.nextInt(moves.length)
          val move = moves(which)
          state = state.afterMove(move)
        }
       }
     }
   }
 }*/

  "a winning state" should {
    "lead to a losing state after the suggested move" in prop{s: State =>
      val statsOption = solution(s.index)
      statsOption match {
        case None => true
        case Some(stats) =>
          if(stats.result == Win) {
            val move = stats.bestMoveOption.get
            val newState = s.afterMove(move)
            solution(newState.index).get.result == Loss
          } else {
            true
          }
      }
    }
  }

  "a losing state" should {
    "have no options left to research and have all of its children winning" in prop{state: State =>
      val statsOption = solution(state.index)
      statsOption match {
        case None => true
        case Some(stats) => {
          stats.result match {
            case Loss => {
              stats.movesLeft shouldEqual(0)
              val moves = rules.legalMoves(state)
              state.isFinal || moves.forall({m =>
                val newState = state.afterMove(m)
                val newStateStats = solution(newState.index).get
                newStateStats.result == Win
              })
            }
            case _ => true
          }
        }

      }
    }
  }}