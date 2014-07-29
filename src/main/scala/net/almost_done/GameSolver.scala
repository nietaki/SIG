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

  val stateStats: Array[Option[StateStats]] = Array.fill(Utils.possibleStatesCount)(None)

  val stateQueue: mutable.Queue[State] = mutable.Queue.empty
  val endStates = Utils.endStates
  endStates.foreach{es: State =>
    stateStats.update(es.index, Some(StateStatsHelper.finalStats))
  }

  val statsUpdated = StateStatsHelper.stateStatsOptionUpdated(rules) _

  stateQueue ++= endStates
  while(stateQueue.nonEmpty) {
    //v - more final state
    val v = stateQueue.dequeue()
    val vStatOption = stateStats(v.index)
    assert(vStatOption.isDefined)
    val vStat = vStatOption.get
    assert(vStat.isSolved)
    val possibleUndoes = rules.legalUndoMoves(v).foreach{vUndo: UndoMove =>
      //w - earlier state, after one undo performed on w
      val w: State = v.beforeUndo(vUndo)
      val wStats = stateStats(w.index)
      //if !wStats.isSolved() ?
      val wStatsUpdated: StateStats = statsUpdated(wStats)(v, vUndo, vStat)
      stateStats(w.index) = Some(wStatsUpdated)
      if (wStatsUpdated.isSolved) {
        stateQueue += w
      }
    }

  }
}
