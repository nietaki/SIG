val combinations = for (
  x <- 0 to 4;
  y <- 0 to 4;
  z <- 0 to 4;
  if(x + y + z == 4)
) yield (x, y, z)

combinations.length

import net.almost_done._


val ss = Utils.randomStartingState

ss.currentPlayerCards
ss.otherPlayerCards

ss.possibleMoves
ss.possibleUndoMoves

val defaultRules = new Rules(Settings.Default)
val simplestRules = new Rules(Settings.Simplest)
val crazyRules = new Rules(Settings.Crazy)

defaultRules.legalMoves(ss)
simplestRules.legalMoves(ss)
crazyRules.legalMoves(ss)

def combinations(startingCounts: List[Int], endingCounts: List[Int] ) = for(
  a <- startingCounts(0) to endingCounts(0);
  b <- startingCounts(1) to endingCounts(1);
  c <- startingCounts(2) to endingCounts(2)
) yield List(a, b, c)

combinations(List(0,7,3), List(1,7,5))


import scala.annotation.tailrec

type SLInt = IndexedSeq[List[Int]]
def combinations2(startingCounts: List[Int], endingCounts: List[Int] ): SLInt = {
  @tailrec
  def inner(acc: SLInt, startingCounts: List[Int], endingCounts: List[Int]): SLInt = {
    (startingCounts, endingCounts) match {
      case (sh :: st, eh :: et) if (sh <= eh) => {
        val newAcc = for(
          ls <- acc;
          last <- (sh to eh)
        ) yield (last :: ls)
        inner(newAcc, st, et)
      }
      case (Nil, Nil) => acc
      case _ => throw new IllegalArgumentException()
    }
  }
  inner(IndexedSeq(List()), startingCounts.reverse, endingCounts.reverse)
}

combinations2(List(0,7,3), List(1,7,5))
