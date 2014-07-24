package net.almost_done

/**
 * Created by nietaki on 24.07.14.
 */
object Implicits {
  implicit def cardSplitIndexedSeqToState(seq: IndexedSeq[CardSplit]): State = State(seq)
}
