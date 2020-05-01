package main.scala.com.tomo.common.domain

case class Player(id: String, name: String)

case class PlayerScore(totalPoints: Int)

object PlayerScore {
  val zero = PlayerScore(0)
}

object PlayerBoard {
  def empty = PlayerBoard(positionCardStack = Map(Top -> CardStack.empty, Middle -> CardStack.empty, Bottom -> CardStack.empty, DroppedCard -> CardStack.empty))

  implicit def map2deck(deck: Map[Position, CardStack]) = PlayerBoard(deck)
}
case class PlayerBoard(positionCardStack: Map[Position, CardStack]) {
  def merge(cards: PlayerBoard): PlayerBoard = PlayerBoard(positionCardStack ++ cards.positionCardStack)
  def add(cards: CardStack, position: Position) = positionCardStack + (position -> cards)

  override def toString: String = positionCardStack.mkString(" | ")
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
case object DroppedCard extends Position(position = -1, nbCards = 4) {
  override def compare(that: Position): Int = Ordering[Int].compare(position, that.position)
}