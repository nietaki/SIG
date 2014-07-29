package net.almost_done
import scala.collection._
/**
 * Created by nietaki on 29.07.14.
 */

/**
 * all the inner variables are named after the article I'm basing the code on, sorry for the inconvenience ;)
 * @param settings
 */
class GameSolver(val settings: Settings) {
  val rules = new Rules(settings)

  val stateStats: Array[StateStats] = Array.fill(Utils.possibleStatesCount)(new StateStats)

  val stateQueue: mutable.Queue[State] = mutable.Queue.empty
  val endStates = Utils.endStates
  endStates.foreach{es: State =>
    val stat = StateStats.finalStats
    assert(stat.isSolved)
  }
  stateQueue ++= endStates
  while(stateQueue.nonEmpty) {
    val v = stateQueue.dequeue()
    val stat = stateStats(v.index)
    assert(stat.isSolved)
    val possibleUndoes = rules.legalUndoMoves(v).foreach{vUndo: UndoMove =>
      val w: State = v.beforeUndo(vUndo)

    }

  }
}
