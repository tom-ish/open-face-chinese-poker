import com.tomo.game.domain.Combination.RoyalFlush

import collection.mutable.Stack
import org.scalatest._

class ScoreSpec extends FlatSpec {

  "A Score" should {
    "transform a list of cards into a Combination" in {
      import com.tomo.game.domain._

      val spade   = "♠"
      val heart   = "♥"
      val club    = "♣"
      val diamond = "♦"

      val royalFlush = CardStack(List(
        Card(Rank.Ace(), spade),
        Card(Rank.Kink(), spade),
        Card(Rank.Queen(), spade),
        Card(Rank.Jack(), spade),
        Card(Rank.Ten(), spade))
      )

      val hand = Score.royalFlush(royalFlush, Top)
      hand shoud be RoyalFlush(Top)
    }
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[String]
    assertThrows[NoSuchElementException] {
      emptyStack.pop()
    }
  }
}