package net.almost_done

import java.io._

import scala.collection._
/**
 * Created by nietaki on 29.07.14.
 */


object GameSolver {

  ///*private*/ val stateStats: Array[Option[StateStats]] = Array.fill(Utils.possibleStatesCount)(None)

  def endStates = Utils.endStates
  def solutionFilename(settings: Settings): String = settings.name + ".solution"

  type Solution = Array[Option[StateStats]]

  def getSolution(settings: Settings): Solution = {
    val filename: String = solutionFilename(settings)
    println(filename)
    val solutionFile = new java.io.File(filename)
    if (solutionFile.exists()) {
      println("reading solution from file")
      val in = new ObjectInputStream(new FileInputStream(solutionFile))
      val stateStats = in.readObject().asInstanceOf[Array[Option[StateStats]]]
      in.close
      println("solution read from file")
      stateStats
    } else {
      println("solving")
      val stateStats = solve(settings)
      println("saving solution to file")
      val out = new ObjectOutputStream(new FileOutputStream(solutionFile))
      out.writeObject(stateStats)
      out.close
      println("solution saved")
      stateStats
    }
  }

  /**
   * all the inner variables are named after the article I'm basing the code on, sorry for the inconvenience ;)
   * @param settings
   */
  private def solve(settings: Settings): Solution = {
    val stateStats: Array[Option[StateStats]] = Array.fill(Utils.possibleStatesCount)(None)
    val rules = new Rules(settings)
    val statsUpdated = StateStatsHelper.stateStatsOptionUpdated(rules) _
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
        if (wStatsOption.fold(true)(! _.isSolved)){ // HMM FIXME
          val wStatsUpdated: StateStats = statsUpdated(wStatsOption)(v, vUndo, vStat)
          stateStats(w.index) = Some(wStatsUpdated)
          if (wStatsUpdated.isSolved) {
            stateQueue += w
            solvedCount +=1
            if(solvedCount % 100000 == 0) {
              println(solvedCount)
              val done = 1.0 * solvedCount / Utils.possibleStatesCount
              println(done)
              println(s"queue length: ${stateQueue.length}")
            }
          }
        } else {
          assert(w.isFinal || wStatsOption.get.isWon)
          println(s"w state says that it is final ${w.isFinal}")
          println(s"wstats says that it is won ${wStatsOption.get.isWon}")
          println()
          //wStatsOption is solved

        }
      }

    }
    stateStats
  }


}
