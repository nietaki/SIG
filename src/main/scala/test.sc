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


