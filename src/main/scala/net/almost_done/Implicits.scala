package net.almost_done

/**
 * Created by nietaki on 24.07.14.
 */
object Implicits {
  implicit def cardSplitIndexedSeq2State(seq: IndexedSeq[CardSplit]): State = State(seq)

  implicit def undo2Move(undo: UndoMove): Move = undo.move
}
