package main.scala.com.tomo.common.domain

case class Player(id: String, name: String)

case class PlayerScore(totalPoints: Int)

object PlayerScore {
  val zero = PlayerScore(0)
}

object PlayerDeck {
  def empty = PlayerDeck(deck = Map(Bottom -> CardStack.empty, Middle -> CardStack.empty, Top -> CardStack.empty))

  implicit def map2deck(deck: Map[Position, CardStack]) = PlayerDeck(deck)
}
case class PlayerDeck(deck: Map[Position, CardStack]) {
  def merge(cards: PlayerDeck): PlayerDeck = PlayerDeck(deck ++ cards.deck)
  def add(cards: CardStack, position: Position) = deck + (position -> cards)

  override def toString: String = deck.mkString(" | ")
}

abstract class Position(val position: Int, val nbCards: Int) extends Ordered[Position]
case object Top extends Position(position = 0, nbCards = 3) {
  override def compare(that: Position): Int = Ordering[Int].compare(position, that.position)
}
case object Middle extends Position(position = 1, nbCards = 5) {
  override def compare(that: Position): Int = Ordering[Int].compare(position, that.position)
}
case object Bottom extends Position(position = 2, nbCards = 5) {
  override def compare(that: Position): Int = Ordering[Int].compare(position, that.position)
}