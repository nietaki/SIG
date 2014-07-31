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
      println(if (usersTurn) "YOUR MOVE" else "COMPUTER's MOVE")
      println(curState)
      println("your cards: " + Utils.cardsRepresentation(curState.playerCards(0)))
      println("opponent's cards: " + Utils.cardsRepresentation(curState.playerCards(1)))
      println("cards on table: " + Utils.cardsRepresentation(curState.tableCards))
      val possibleMoves = rules.legalMoves(curState)
      val stateStat = solution(curState.index)
      possibleMoves.zipWithIndex.foreach({ case (move, idx) => println(s"$idx: ${move.moveDescription}")})
      println(s"stateStats: $stateStat")
      if (usersTurn) {
        val choice = InteractivePlayer.getPlayerChoice(possibleMoves.length, "pick a move>")
        val newState = curState.afterMove(possibleMoves(choice))
        makeMove(newState, !usersTurn)
      } else {
        //computer's move
        //val choice = InteractivePlayer.getPlayerChoice(possibleMoves.length, "pick computer's move>")
        //val newState = curState.afterMove(possibleMoves(choice))
        val bestMove = stateStat.get.bestMoveOption.get
        val newState = curState.afterMove(bestMove)
        makeMove(newState, !usersTurn)
      }

    } else {
      println("GAME HAS FINISHED!")
      val who = if(usersTurn) "computer" else "user"
      println(s"$who has won!")
      println()
      println()
      println()
      println()
      println()
    }
  }
}
