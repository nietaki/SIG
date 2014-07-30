package net.almost_done

import java.io._

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
  //TODO elliminate the state here
  /*private*/ val stateStats: Array[Option[StateStats]] = Array.fill(Utils.possibleStatesCount)(None)

  val statsUpdated = StateStatsHelper.stateStatsOptionUpdated(rules) _

  def endStates = Utils.endStates

  def solutionFilename: String = settings.hashCode().toString + ".solution"

  def getSolution: Array[Option[StateStats]] = {
    val solutionFile = new java.io.File(solutionFilename)
    if (solutionFile.exists()) {
      println("reading solution from file")
      val in = new ObjectInputStream(new FileInputStream(solutionFile))
      val stateStats = in.readObject().asInstanceOf[Array[Option[StateStats]]]
      in.close
      println("solution read from file")
    } else {
      println("solving")
      solve()
      println("saving solution to file")
      val out = new ObjectOutputStream(new FileOutputStream(solutionFile))
      out.writeObject(stateStats)
      out.close
      println("solution saved")
    }
    stateStats
  }
  def solve(): Array[Option[StateStats]] = {

    val stateQueue: mutable.Queue[State] = mutable.Queue.empty
    endStates.foreach{es: State =>
      stateStats.update(es.index, Some(StateStatsHelper.finalStats))
    }

    var solvedCount = 0
    stateQueue ++= endStates
    while(stateQueue.nonEmpty) {
      //v - more final state
      val v = stateQueue.dequeue()
      val vStat = stateStats(v.index).get
      assert(vStat.isSolved)
      val possibleUndoes = rules.legalUndoMoves(v).foreach{vUndo: UndoMove =>
        //w - earlier state, after one undo performed on v
        val w: State = v.beforeUndo(vUndo)
        val wStatsOption = stateStats(w.index)
        /*
        the W state shouldn't be solved in an usual situation, since we know all its child states and we should reach it
        from each of them once. The only other situation is when backing to a final state, which, by definition, shouldn't
         have any children. Let's test that
         */
        if (wStatsOption.fold(true)(! _.isSolved)){ // HMM
          val wStatsUpdated: StateStats = statsUpdated(wStatsOption)(v, vUndo, vStat)
          stateStats(w.index) = Some(wStatsUpdated)
          if (wStatsUpdated.isSolved) {
            stateQueue += w
            solvedCount +=1
            if(solvedCount % 1000 == 0) {
              println(solvedCount)
              val done = 1.0 * solvedCount / Utils.possibleStatesCount
              println(done)
              println(s"queue length: ${stateQueue.length}")
            }
          }
        } else {
          //wStatsOption is solved

        }
      }

    }
    stateStats
  }


}
