package main.scala.com.tomo.common.domain

import main.scala.com.tomo.common.Utils

import scala.util.Random

abstract class Suit(val name: String, val shortName: String) extends Serializable
abstract class Rank(val value: Int, val name: String, val shortName: String) extends Serializable with Ordered[Rank]

object Suit {
  case class Clubs() extends Suit("Clubs", "♣")
  case class Spades() extends Suit("Spades", "♠")
  case class Diamonds() extends Suit("Diamonds", "♦")
  case class Hearts() extends Suit("Hearts", "♥")

  def all: List[Suit] = List(Clubs(), Spades(), Diamonds(), Hearts())

  implicit def string2suit(s: String) : Suit = s match {
    case "♣" => Clubs()
    case "♠" => Spades()
    case "♦" => Diamonds()
    case "♥" => Hearts()
    case _ => throw new RuntimeException(s"Unknown suit $s")
  }
}

object Rank {
  case class Ace() extends Rank(14, "Ace", "A") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Two() extends Rank(2, "Two", "2") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Three() extends Rank(3, "Three", "3") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Four() extends Rank(4, "Four", "4") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Five() extends Rank(5, "Five", "5") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Six() extends Rank(6, "Six", "6") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Seven() extends Rank(7, "Seven", "7") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Eight() extends Rank(8, "Eight", "8") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Nine() extends Rank(9, "Nine", "9") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Ten() extends Rank(10, "Ten", "10") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Jack() extends Rank(11, "Jack", "J") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class Queen() extends Rank(12, "Queen", "Q") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }
  case class King() extends Rank(13, "King", "K") {
    override def compare(that: Rank): Int =
      Ordering[Int].compare(value, that.value)
  }

  def all: List[Rank] = List(Ace(), Two(), Three(), Four(), Five(), Six(), Seven(), Eight(), Nine(), Ten(), Jack(), Queen(), King())

  implicit def get(i: Int) = i match {
    case 1 => Ace() // Avoid to use this, use 14 instead
    case 2 => Two()
    case 3 => Three()
    case 4 => Four()
    case 5 => Five()
    case 6 => Six()
    case 7 => Seven()
    case 8 => Eight()
    case 9 => Nine()
    case 10 => Ten()
    case 11 => Jack()
    case 12 => Queen()
    case 13 => King()
    case 14 => Ace()
    case _ => throw new RuntimeException("Error: card number undefined")
  }

  implicit def string2rank(s: String) : Rank = s match {
    case "A" => Ace()
    case "2" => Two()
    case "3" => Three()
    case "4" => Four()
    case "5" => Five()
    case "6" => Six()
    case "7" => Seven()
    case "8" => Eight()
    case "9" => Nine()
    case "10" => Ten()
    case "J" => Jack()
    case "Q" => Queen()
    case "K" => King()
    case _ => throw new RuntimeException(s"Unknown rank $s")
  }
}

object Card {
  private val pattern = "^([AJKQ2-9])\\s*([♣♠♦♥])$".r

  def fullDeck: List[Card] = for {
    suit <- Suit.all
    rank <- Rank.all
  } yield Card(rank, suit)

  implicit def string2card(s: String): Card = s match {
    case pattern(rank, suit) => Card(rank, suit)
    case _ => throw new RuntimeException(s"Invalid card string $s")
  }
}

case class Card(rank: Rank, suit: Suit) extends Ordered[Card] {
  def name(): String = s"${rank.name} of ${suit.name}"
  def shortName(): String = s"${rank.shortName}${suit.shortName}"

  override def compare(that: Card): Int = {
    Ordering[Int].compare(this.rank.value, that.rank.value)
  }

  override def toString: String = shortName()
}


object CardStack {
  val empty: CardStack = CardStack(List())
  def sorted: CardStack = CardStack(Card.fullDeck)
  def shuffled: CardStack = CardStack(Random.shuffle(Card.fullDeck))

  implicit def card2stack(cards: List[Card]): CardStack = CardStack(cards)
}

case class CardStack(cards: List[Card]) {
  def removed(card: Card): CardStack = CardStack(Utils.removeLast(cards, card))
  def removed(cards: Seq[Card]): CardStack = cards.foldLeft(this)((stack, card) => stack.removed(card))

  def added(card: Card): CardStack = CardStack(cards :+ card)

  def isEmpty = cards.isEmpty

  override def toString: String = cards.mkString(", ")
}
