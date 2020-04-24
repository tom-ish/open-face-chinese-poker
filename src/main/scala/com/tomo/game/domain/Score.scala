package com.tomo.game.domain

import scala.collection.immutable.SortedSet

object Score {
  def apply(deck: CardStack): Score = new Score(deck)

  def royalFlush(deck: CardStack): Option[Boolean] = {
    straightFlush(deck) match {
      case Some(card) if card.rank == Rank.Ace() => Option(true)
      case _ => Option(false)
    }
  }

  def straightFlush(deck: CardStack): Option[Card] = {
    straight(deck) match {
      case Some(card) =>
        flush(deck) match {
          case Some(_) =>
            Option(card)
          case None =>
            None
        }
      case None =>
        None
    }
  }

  def fourOfAKind(deck: CardStack): Option[(Card, Card)] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 2 =>
        if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet.head.rank == cardsSet(2).rank)
          Option((cardsSet.head, cardsSet.tail.head))
        else if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet.head.rank != cardsSet(2).rank)
          Option((cardsSet.tail.head, cardsSet.head))
        else
          None
      case _ =>
        None
    }
  }

  def fullHouse(deck: CardStack): Option[(Card, Card)] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 2 =>
        if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet.head.rank == cardsSet(2).rank)
          Option((cardsSet.head, cardsSet.tail.head))
        else if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet.head.rank != cardsSet(2).rank)
          Option((cardsSet.tail.head, cardsSet.head))
        else
          None
      case _ => None
    }
  }

  def flush(deck: CardStack): Option[Seq[Card]] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val isFlush = cardsSet.sliding(2).forall {
      case current::next::Nil =>
        current.suit == next.suit
    }
    if (isFlush) Option(cardsSet)
    else None
  }

  def straight(deck: CardStack): Option[Card] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    cardsSet.size match {
      case 5 =>
        if(cardsSet(4).rank == Rank.Ace() && cardsSet.head.rank == Rank.Two() && cardsSet(1).rank == Rank.Three() &&
          cardsSet(2).rank == Rank.Four() && cardsSet(3).rank == Rank.Five())
          Option(cardsSet.head)
        else if (cardsSet.head.rank.value + 1 == cardsSet(1).rank.value
          && cardsSet(1).rank.value + 1 == cardsSet(2).rank.value
          && cardsSet(2).rank.value + 1 == cardsSet(3).rank.value
          && cardsSet(3).rank.value + 1 == cardsSet(4).rank.value)
          Option(cardsSet(4))
        else
          None
      case _ => None
    }
  }

  def threeOfAKind(deck: CardStack): Option[(Card, Seq[Card])] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 3 =>
        if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet.head.rank == cardsSet(2).rank)
          Option(cardsSet.head, Seq(cardsSet(1), cardsSet(2), cardsSet(3), cardsSet.tail.head))
        else if(cardsSet(1).rank == cardsSet(2).rank
        && cardsSet(1).rank == cardsSet(3).rank)
          Option(cardsSet(1), Seq(cardsSet.head, cardsSet(1), cardsSet(2), cardsSet.tail.head))
        else if(cardsSet(2).rank == cardsSet(3).rank
        && cardsSet(2).rank == cardsSet.tail.head.rank)
          Option(cardsSet.tail.head, Seq(cardsSet.head, cardsSet(1), cardsSet(2), cardsSet(3)))
        else
          None
      case _ => None
    }
  }

  def twoPairs(deck: CardStack): Option[(Card, Card, Card)] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 3 =>
        if(cardsSet.head.rank == cardsSet(1).rank
          && cardsSet(2).rank == cardsSet(3).rank)
          Option((cardsSet.head, cardsSet(2), cardsSet.tail.head))
        else if(cardsSet.head.rank != cardsSet(1).rank
          && cardsSet(1).rank == cardsSet(2).rank
          && cardsSet(3).rank == cardsSet.tail.head.rank)
          Option((cardsSet(1), cardsSet(3), cardsSet.head))
        else
          None
      case _ => None
    }
  }

  def pair(deck: CardStack): Option[(Card, Seq[Card])] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 4 =>
        if(cardsSet.head.rank == cardsSet(1).rank) Option((cardsSet.head, Seq(cardsSet(2), cardsSet(3), cardsSet.tail.head)))
        else if(cardsSet(1).rank == cardsSet(2).rank) Option((cardsSet(1), Seq(cardsSet.head, cardsSet(3), cardsSet.tail.head)))
        else if(cardsSet(2).rank == cardsSet(3).rank) Option((cardsSet(2), Seq(cardsSet.head, cardsSet(1), cardsSet.tail.head)))
        else if(cardsSet(3).rank == cardsSet.tail.head.rank) Option((cardsSet(3), Seq(cardsSet.head, cardsSet(1), cardsSet(2))))
        else None
      case _ => None
    }
  }
}

case class Score(deck: CardStack) {
  def compute: Int = 0
}

abstract class Combination(val name: String, val strength: Int, val score: Int)
object Combination {
  case class FourOfAKind(value: Int) extends Combination(name = "FourOfAKind", strength = 8, score = value)
  case class FullHouse(value: Int) extends Combination(name = "FullHouse", strength = 7, score = value)
  case class Flush(value: Int) extends Combination(name = "Flush", strength = 6, score = value)
  case class Straight(value: Int) extends Combination(name = "Straight", strength = 5, score = value)
  case class ThreeOfAKind(value: Int) extends Combination(name = "ThreeOfAKind", strength = 4, score = value)
  case class TwoPair(value: Int) extends Combination(name = "TwoPair", strength = 3, score = value)
  case class SinglePair(value: Int) extends Combination(name = "SinglePair", strength = 2, score = value)
  case class SingleCard(value: Int) extends Combination(name = "SingleCard", strength = 1, score = value)

  def transform(cards: CardStack): Combination = ???
}