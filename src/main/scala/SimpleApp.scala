import net.almost_done._

import scala.util.Random

/**
 * Created by nietaki on 3/6/14.
 */
object SimpleApp {

  def main(args: Array[String]): Unit = {
    val settings = Settings.Simplest
    val solution: GameSolver.Solution = GameSolver.getSolution(settings)
    val player = new InteractivePlayer(settings, solution)
    val startingState = Utils.randomStartingState

    while(true) {
      val playerStarting = Random.nextBoolean()
      player.makeMove(startingState, playerStarting)
    }

  }

}
