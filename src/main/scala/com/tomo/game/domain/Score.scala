package com.tomo.game.domain

object Score {

}

case class Score() {

}

abstract class Combinaison(val name: String, val strength: Int, val value: Int)
object Combinaison {
  case class FourOfAKind(score: Int) extends Combinaison(name = "FourOfAKind", strength = 8, value = score)
  case class FullHouse(score: Int) extends Combinaison(name = "FullHouse", strength = 7, value = score)
  case class Flush(score: Int) extends Combinaison(name = "Flush", strength = 6, value = score)
  case class Straight(score: Int) extends Combinaison(name = "Straight", strength = 5, value = score)
  case class ThreeOfAKind(score: Int) extends Combinaison(name = "ThreeOfAKind", strength = 4, value = score)
  case class TwoPair(score: Int) extends Combinaison(name = "TwoPair", strength = 3, value = score)
  case class SinglePair(score: Int) extends Combinaison(name = "SinglePair", strength = 2, value = score)
  case class SingleCard(score: Int) extends Combinaison(name = "SingleCard", strength = 1, value = score)
}

object Hand
case class Hand(cards: List[Card]) {

}