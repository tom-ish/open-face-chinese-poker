package com.tomo.game.domain

import com.tomo.game.domain.Combination.{Flush, FourOfAKind, FullHouse, HighCard, RoyalFlush, SinglePair, Straight, StraightFlush, ThreeOfAKind, TwoPair}

import scala.collection.immutable.SortedSet

object Score {
  def apply(deck: CardStack): Score = new Score(deck)

  def royalFlush(deck: CardStack, position: Position): Option[Combination] = {
    straightFlush(deck, position) match {
      case Some(royalFlush) if royalFlush.score == 60 => Option(RoyalFlush(position))
      case _ => None
    }
  }

  def straightFlush(deck: CardStack, position: Position): Option[Combination] = {
    straight(deck, position) match {
      case Some(_) =>
        flush(deck, position) match {
          case Some(_) =>
            Option(StraightFlush(deck.cards.map(_.rank.value).sum, position))
          case None =>
            None
        }
      case None =>
        None
    }
  }

  def fourOfAKind(deck: CardStack, position: Position): Option[Combination] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 2 =>
        /* XXXXO */
        if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet.head.rank == cardsSet(2).rank)
          Option(FourOfAKind(cardsSet.head.rank.value * 4, cardsSet.tail.head.rank.value, position))
        /* XOOOO*/
        else if(cardsSet.head.rank != cardsSet(1).rank
        && cardsSet.tail.head.rank == cardsSet(2).rank)
          Option(FourOfAKind(cardsSet.tail.head.rank.value * 4, cardsSet.head.rank.value, position))
        else
          None
      case _ =>
        None
    }
  }

  def fullHouse(deck: CardStack, position: Position): Option[Combination] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 2 =>
        /* XXXOO */
        if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet.head.rank == cardsSet(2).rank)
          Option(FullHouse(cardsSet.head.rank.value * 3, cardsSet.tail.head.rank.value * 2, position))
        /* XXOOO */
        else if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet.head.rank != cardsSet(2).rank)
          Option(FullHouse(cardsSet.tail.head.rank.value * 3, cardsSet.head.rank.value * 2, position))
        else
          None
      case _ => None
    }
  }

  def flush(deck: CardStack, position: Position): Option[Combination] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val isFlush = cardsSet.sliding(2).forall {
      case current::next::Nil =>
        current.suit == next.suit
    }
    if (isFlush) Option(Flush(deck.cards.map(_.rank.value).sum, position))
    else None
  }

  def straight(deck: CardStack, position: Position): Option[Combination] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    cardsSet.size match {
      case 5 =>
        if(cardsSet(4).rank == Rank.Ace() && cardsSet.head.rank == Rank.Two() && cardsSet(1).rank == Rank.Three() &&
          cardsSet(2).rank == Rank.Four() && cardsSet(3).rank == Rank.Five())
          Option(Straight(deck.cards.map {
            case card if card.rank == Rank.Ace() => 1
            case card => card.rank.value
          }.sum, position))
        else if (cardsSet.head.rank.value + 1 == cardsSet(1).rank.value
          && cardsSet(1).rank.value + 1 == cardsSet(2).rank.value
          && cardsSet(2).rank.value + 1 == cardsSet(3).rank.value
          && cardsSet(3).rank.value + 1 == cardsSet(4).rank.value)
          Option(Straight(deck.cards.map(_.rank.value).sum, position))
        else
          None
      case _ => None
    }
  }

  def threeOfAKind(deck: CardStack, position: Position): Option[Combination] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 3 =>
        /* XXXOR */
        if(cardsSet.head.rank == cardsSet(1).rank && cardsSet.head.rank == cardsSet(2).rank
        && cardsSet.tail.head.rank != cardsSet(3).rank)
          Option(ThreeOfAKind(
            cardsSet.head.rank.value * 3,
            cardsSet(3).rank.value + cardsSet.tail.head.rank.value,
            position))
        /* XOOOR */
        else if(cardsSet(1).rank == cardsSet(2).rank
        && cardsSet(1).rank == cardsSet(3).rank)
          Option(ThreeOfAKind(
            cardsSet(1).rank.value * 3,
            cardsSet.head.rank.value + cardsSet.tail.head.rank.value,
            position))
        /* XORRR */
        else if(cardsSet(2).rank == cardsSet(3).rank
        && cardsSet(2).rank == cardsSet.tail.head.rank)
          Option(ThreeOfAKind(
            cardsSet.tail.head.rank.value * 3,
            cardsSet.head.rank.value + cardsSet(1).rank.value,
            position))
        else
          None
      case _ => None
    }
  }

  def twoPairs(deck: CardStack, position: Position): Option[Combination] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 3 =>
        /* XXOOR */
        if(cardsSet.head.rank == cardsSet(1).rank
          && cardsSet(2).rank == cardsSet(3).rank)
          Option(TwoPair(
            cardsSet(2).rank.value * 2,
            cardsSet.head.rank.value * 2,
            cardsSet.tail.head.rank.value,
            position))
        /* XOORR */
        else if(cardsSet.head.rank != cardsSet(1).rank
          && cardsSet(1).rank == cardsSet(2).rank
          && cardsSet(3).rank == cardsSet.tail.head.rank)
          Option(TwoPair(
            cardsSet(3).rank.value * 2,
            cardsSet(1).rank.value * 2,
            cardsSet.head.rank.value,
            position))
        /* XXORR */
        else if(cardsSet.head.rank == cardsSet(1).rank
        && cardsSet(3).rank == cardsSet.tail.head.rank)
          Option(TwoPair(
            cardsSet.tail.head.rank.value * 2,
            cardsSet.head.rank.value * 2,
            cardsSet(2).rank.value,
            position))
        else
          None
      case _ => None
    }
  }

  def pair(deck: CardStack, position: Position): Option[Combination] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 4 =>
        if(cardsSet.head.rank == cardsSet(1).rank)
          Option(SinglePair(
            cardsSet.head.rank.value * 2,
            cardsSet(2).rank.value + cardsSet(3).rank.value + cardsSet.tail.head.rank.value,
            position))
        else if(cardsSet(1).rank == cardsSet(2).rank)
          Option(SinglePair(
            cardsSet(1).rank.value * 2,
            cardsSet.head.rank.value + cardsSet(3).rank.value + cardsSet.tail.head.rank.value,
            position))
        else if(cardsSet(2).rank == cardsSet(3).rank)
          Option(SinglePair(
            cardsSet(2).rank.value,
            cardsSet.head.rank.value + cardsSet(1).rank.value + cardsSet.tail.head.rank.value,
            position))
        else if(cardsSet(3).rank == cardsSet.tail.head.rank)
          Option(SinglePair(
            cardsSet(3).rank.value,
            cardsSet.head.rank.value + cardsSet(1).rank.value + cardsSet(2).rank.value,
            position))
        else None
      case _ => None
    }
  }

  def highCard(deck: CardStack, position: Position): Option[Combination] = {
    val cardsSet = (SortedSet() ++ deck.cards.toSet).toSeq
    val ranks = (SortedSet() ++ deck.cards.map(_.rank.value).toSet).toSeq

    ranks.size match {
      case 5 =>
        Option(HighCard(cardsSet.map(_.rank.value).size, position))
      case _ =>
        None
    }
  }
}

case class Score(deck: CardStack) {
  def compute: Int = 0
}

abstract class Combination(val name: String, val strength: Int, val score: Int, position: Position) {
  def royalties: Int
}
object Combination {
  case class RoyalFlush(row: Position)
    extends Combination(name = "RoyalFlush", strength = 10, position = row,
      score = 60) {
    override def royalties: Int = row match {
      case Bottom => 25
      case Middle => 50
      case Top => throw new RuntimeException("Error: cannot have RoyalFlush in Top Position")
    }
  }
  case class StraightFlush(scoreHighs: Int, row: Position)
    extends Combination(name = "StraightFlush", strength = 9, position = row,
      score = scoreHighs) {
    override def royalties: Int = row match {
      case Bottom => 15
      case Middle => 30
      case Top => throw new RuntimeException("Error: cannot have StraightFlush in Top Position")
    }
  }
  case class FourOfAKind(scoreHighs: Int, scoreLows: Int, row: Position)
    extends Combination(name = "FourOfAKind", strength = 8, position = row,
      score = scoreHighs + scoreLows) {
    override def royalties: Int = row match {
      case Bottom => 10
      case Middle => 20
      case Top => throw new RuntimeException("Error: cannot have FourOfAKind in Top Position")
    }
  }
  case class FullHouse(scoreHighs: Int, scoreLows: Int, row: Position)
    extends Combination(name = "FullHouse", strength = 7, position = row,
      score = scoreHighs + scoreLows) {
    override def royalties: Int = row match {
      case Bottom => 6
      case Middle => 12
      case Top => throw new RuntimeException("Error: cannot have FullHouse in Top Position")
    }
  }
  case class Flush(scoreHighs: Int, row: Position)
    extends Combination(name = "Flush", strength = 6, position = row,
      score = scoreHighs) {
    override def royalties: Int = row match {
      case Bottom => 4
      case Middle => 8
      case Top => throw new RuntimeException("Error: cannot have Flush in Top Position")
    }
  }
  case class Straight(scoreHighs: Int, row: Position)
    extends Combination(name = "Straight", strength = 5, position = row,
      score = scoreHighs) {
    override def royalties: Int = row match {
      case Bottom => 2
      case Middle => 4
      case Top => throw new RuntimeException("Error: cannot have Straight in Top Position")
    }
  }
  case class ThreeOfAKind(scoreHighs: Int, scoreLows: Int, row: Position)
    extends Combination(name = "ThreeOfAKind", strength = 4, position = row,
      score = scoreHighs + scoreLows) {
    override def royalties: Int = row match {
      case Bottom => 0
      case Middle => 2
      case Top => Rank.get(scoreHighs/3).value + 8
    }
  }
  case class TwoPair(scoreFirstHighs: Int, scoreSecondHighs: Int, scoreLows: Int, row: Position)
    extends Combination(name = "TwoPair", strength = 3, position = row,
      score = scoreFirstHighs + scoreSecondHighs + scoreLows) {
    override def royalties: Int = 0
  }
  case class SinglePair(scoreHighs: Int, scoreLows: Int, row: Position)
    extends Combination(name = "SinglePair", strength = 2, position = row,
      score = scoreHighs + scoreLows) {
    override def royalties: Int = row match {
      case Top if scoreHighs/2 > 5 => Rank.get(scoreHighs/2).value - 5
      case _ => 0
    }
  }
  case class HighCard(scoreHighs: Int, row: Position) extends
    Combination(name = "HighCard", strength = 1, position = row, score = scoreHighs) {
    override def royalties: Int = 0
  }
  /*
  case class SingleCard(score: Int, row: Position) extends Combination(name = "SingleCard", strength = 1, score = score, position = row) {
    override def royalties: Int = 0
  }*/

  def transform(cards: CardStack, position: Position): Option[Combination] = {
    val combination = Score.royalFlush(cards, position)
      .getOrElse(Score.straightFlush(cards, position)
        .getOrElse(Score.fourOfAKind(cards, position)
          .getOrElse(Score.fullHouse(cards, position)
            .getOrElse(Score.threeOfAKind(cards, position)
              .getOrElse(Score.twoPairs(cards, position)
                .getOrElse(Score.pair(cards, position)
                  .getOrElse(Score.highCard(cards, position)))
              )
            )
          )
        )
      )
    combination.asInstanceOf[Option[Combination]]
  }
}