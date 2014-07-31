import net.almost_done._

import scala.util.Random

/**
 * Created by nietaki on 3/6/14.
 */
object SimpleApp {

  def main(args: Array[String]): Unit = {
    println("choose game version: 0 - simplest, 1 - default, 2 - crazy")
    val choice = InteractivePlayer.getPlayerChoice(3)
    val settingsSeq = List(Settings.Simplest, Settings.Default, Settings.Crazy)
    val settings = settingsSeq(choice)
    val solution: GameSolver.Solution = GameSolver.getSolution(settings)
    val player = new InteractivePlayer(settings, solution)
    val startingState = Utils.randomStartingState

    while(true) {
      val playerStarting = Random.nextBoolean()
      player.makeMove(startingState, playerStarting)
    }

  }

}
