package main.scala.com.tomo.common.domain

import main.scala.com.tomo.common.domain.Hand._

/*
trait commonScore {
}
object Hand3 {
  type Hand = (Card, Card, Card)
  def apply(cardStack: CardStack): Hand = {
    val List(card1, card2, card3) = cardStack.cards
    (card1, card2, card3)
  }
}
*/

object Hand {
  type Hand = (Card, Card, Card, Card, Card)

  def apply(cardStack: CardStack): Hand = {
    val List(card1, card2, card3, card4, card5) = cardStack.cards
    (card1, card2, card3, card4, card5)
  }

  def toRanks(hand: Hand): List[Rank] = {
    val (card1, card2, card3, card4, card5) = hand
    List(card1.rank, card2.rank, card3.rank, card4.rank, card5.rank).sorted
  }

  def toRankValues(hand: Hand): List[Int] = {
    val (card1, card2, card3, card4, card5) = hand
    List(card1.rank.value, card2.rank.value, card3.rank.value, card4.rank.value, card5.rank.value).sorted
  }

  def toRankSizes(hand: Hand): List[Int] = {
    toRanks(hand).groupBy(identity).map {
      case (_, listOfRanks) => listOfRanks.size
    }.toList
  }

  def toSuits(hand: Hand): List[Suit] = {
    val (card1, card2, card3, card4, card5) = hand
    List(card1.suit, card2.suit, card3.suit, card4.suit, card5.suit)
  }

  def getHighsLows(hand: Hand, highs: (Int, Int), lows: (Int, Int)) = {
    toRanks(hand).groupBy(identity).map {
      case (rank, listOfRank) if listOfRank.size == highs._1 =>
        rank.value * highs._2
      case (rank, listOfRank) if listOfRank.size == lows._1 =>
        rank.value * lows._2
      case _ =>
        0
    }.sum
  }

  def getHighsValue(hand: Hand, highs: Int) = {
    toRankValues(hand).groupBy(identity).filter (_._2.size == highs).head._1
  }

  def isRoyalFlush(hand: Hand) = {
    val ranks = toRanks(hand)
    isStraightFlush(hand) && ranks.contains(Rank.Ace()) && ranks.contains(Rank.King)
  }

  def isStraightFlush(hand: Hand) = isFlush(hand) && isStraight(hand)

  def isFourOfAKind(hand: Hand) = toRankSizes(hand).contains(4)

  def isFullHouse(hand: Hand): Boolean = {
    val rankSizes = toRankSizes(hand)
    rankSizes.contains(3) && rankSizes.contains(2)
  }

  def isFlush(hand: Hand): Boolean = toSuits(hand).forall(_ == hand._1.suit)

  def isStraight(hand: Hand): Boolean = {
    val rankValues = toRankValues(hand)
    val minRank = rankValues.min
    rankValues.toSet == minRank.to(minRank + 4).toSet ||
      rankValues.sorted.toSet == List(2 to 5, 14).toSet // corner case: Straight from Ace to Five
  }

  def isThreeOfAKind(hand: Hand): Boolean = toRankSizes(hand).contains(3)

  def isTwoPairs(hand: Hand): Boolean = toRankSizes(hand).count(_ == 2) == 2

  def isSinglePair(hand: Hand): Boolean = toRankSizes(hand).count(_ == 2) == 1

  def evaluate(hand: Hand, position: Position): HandType = {
    if (isRoyalFlush(hand)) HandType.RoyalFlush(position)
    else if (isStraightFlush(hand)) HandType.StraightFlush(hand, position)
    else if (isFourOfAKind(hand)) HandType.FourOfAKind(hand, position)
    else if (isFullHouse(hand)) HandType.FullHouse(hand, position)
    else if (isFlush(hand)) HandType.Flush(hand, position)
    else if (isStraight(hand)) HandType.Straight(hand, position)
    else if (isThreeOfAKind(hand)) HandType.ThreeOfAKind(hand, position)
    else if (isTwoPairs(hand)) HandType.TwoPairs(hand, position)
    else if (isSinglePair(hand)) HandType.SinglePair(hand, position)
    else HandType.HighCards(hand, position)
  }
}

abstract class HandType(val name: String, val strength: Int, position: Position) {
  def royalties: Int
  def evaluateScore(hand: Hand): Int
}

object HandType {
  case class RoyalFlush(row: Position)
    extends HandType(name = "RoyalFlush", strength = 10, position = row) {
    override def royalties: Int = row match {
      case Bottom => 25
      case Middle => 50
      case Top => throw new RuntimeException("Error: cannot have RoyalFlush in Top Position")
    }

    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      toRankValues(hand).sum
  }
  case class StraightFlush(hand: Hand, row: Position)
    extends HandType(name = "StraightFlush", strength = 9, position = row) {
    override def royalties: Int = row match {
      case Bottom => 15
      case Middle => 30
      case Top => throw new RuntimeException("Error: cannot have StraightFlush in Top Position")
    }

    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      toRankValues(hand).sum
  }
  case class FourOfAKind(hand: Hand, row: Position)
    extends HandType(name = "FourOfAKind", strength = 8, position = row) {
    override def royalties: Int = row match {
      case Bottom => 10
      case Middle => 20
      case Top => throw new RuntimeException("Error: cannot have FourOfAKind in Top Position")
    }

    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      getHighsLows(hand, highs = (4, 100), lows = (1, 1))
  }
  case class FullHouse(hand: Hand, row: Position)
    extends HandType(name = "FullHouse", strength = 7, position = row) {
    override def royalties: Int = row match {
      case Bottom => 6
      case Middle => 12
      case Top => throw new RuntimeException("Error: cannot have FullHouse in Top Position")
    }

    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      getHighsLows(hand, highs = (3, 1000), lows = (2, 100))
  }
  case class Flush(hand: Hand, row: Position)
    extends HandType(name = "Flush", strength = 6, position = row) {
    override def royalties: Int = row match {
      case Bottom => 4
      case Middle => 8
      case Top => throw new RuntimeException("Error: cannot have Flush in Top Position")
    }

    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      toRankValues(hand).sum
  }
  case class Straight(hand: Hand, row: Position)
    extends HandType(name = "Straight", strength = 5, position = row) {
    override def royalties: Int = row match {
      case Bottom => 2
      case Middle => 4
      case Top => throw new RuntimeException("Error: cannot have Straight in Top Position")
    }

    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      toRankValues(hand).sum
  }
  case class ThreeOfAKind(hand: Hand, row: Position)
    extends HandType(name = "ThreeOfAKind", strength = 4, position = row) {
    override def royalties: Int = row match {
      case Bottom => 0
      case Middle => 2
      case Top => getHighsValue(hand, 3) + 8
    }

    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      getHighsLows(hand, highs = (3, 100), lows = (1, 1))
  }
  case class TwoPairs(hand: Hand, row: Position)
    extends HandType(name = "TwoPair", strength = 3, position = row) {
    override def royalties: Int = 0
    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int = ???
  }
  case class SinglePair(hand: Hand, row: Position)
    extends HandType(name = "SinglePair", strength = 2, position = row) {
    override def royalties: Int = row match {
      case Top if getHighsValue(hand, 2) > 5 => getHighsValue(hand, 2) - 5
      case _ => 0
    }
    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      getHighsLows(hand, (2, 100), (1, 1))
  }
  case class HighCards(hand: Hand, row: Position) extends
    HandType(name = "HighCard", strength = 1, position = row) {
    override def royalties: Int = 0
    override def evaluateScore(hand: (Card, Card, Card, Card, Card)): Int =
      toRankValues(hand).sum
  }
}