package net.almost_done

import scala.annotation.tailrec
import scala.util._

/**
 * Created by nietaki on 30.07.14.
 */
object InteractivePlayer {
  @tailrec
  def getPlayerChoice(optionCount: Int, prompt: String = "prompt"): Int = {
    val t: Try[Int] = Try(Console.readInt()).flatMap[Int]({i: Int =>
      if( i>=0 && i < optionCount)
        Success(i)
      else
        Failure(new IndexOutOfBoundsException())})
    t match {
      case Success(i) => i
      case Failure(_) => getPlayerChoice(optionCount, prompt)
    }
  }
}

class InteractivePlayer(settings: Settings, solution: GameSolver.Solution) {
  val rules = new Rules(settings)
  def makeMove(curState: State, usersTurn: Boolean): Unit = {
    if (!curState.isFinal) {
      println("YOUR MOVE")
      println(curState)
      println("your cards: " + Utils.cardsRepresentation(curState.playerCards(0)))
      println("opponent's cards: " + Utils.cardsRepresentation(curState.playerCards(1)))
      println("cards on table: " + Utils.cardsRepresentation(curState.tableCards))
      val possibleMoves = rules.legalMoves(curState)
      if (usersTurn) {
        possibleMoves.zipWithIndex.foreach({ case (move, idx) => println(s"$idx: ${move.moveDescription}")})
        val choice = InteractivePlayer.getPlayerChoice(possibleMoves.length)
        val newState = curState.afterMove(possibleMoves(choice))
        makeMove(newState, true) //FIXME for computer playing as well
      } else {
        //computer's move
      }
    } else {
      println("GAME HAS FINISHED!")
      val who = if(usersTurn) "computer" else "user"
      println(s"$who has won!")
    }
  }
}
