package test

import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.matcher.TraversableMatchers
import org.specs2.mutable._

import net.almost_done._

/**
 * Created by nietaki on 04.07.14.
 */
class DeckSpec extends Specification with TraversableMatchers with ScalaCheck  {
  "Deck" should {
    "construct a correct random deck" in {
      val state = Utils.randomStartingState
      state.cardSplits.length == 6
    }
  }

}