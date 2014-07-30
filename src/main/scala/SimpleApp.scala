import net.almost_done._

/**
 * Created by nietaki on 3/6/14.
 */
object SimpleApp {

  def main(args: Array[String]): Unit = {
     
    val gs = new GameSolver(Settings.Simplest)
    gs.getSolution
  }

}
