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
  type Hand = List[Card]

  def apply(cardStack: CardStack): Hand = cardStack.cards

  def toRanks(hand: Hand): List[Rank] = hand.map(_.rank).sortBy(_.value)

  def toRankValues(hand: Hand): List[Int] = hand.map(_.rank.value).sorted

  def toRankSizes(hand: Hand): List[Int] = {
    toRanks(hand).reverse.groupBy(identity).map {
      case (_, listOfRanks) =>
        listOfRanks.size
    }.toList
  }

  def toSuits(hand: Hand): List[Suit] = hand map (_.suit)


  def getHighsLows(hand: Hand, highs: (Int, Int), lows: (Int, Int)) = {
    toRanks(hand).reverse.groupBy(identity).map {
      case (rank, listOfRank) if listOfRank.size == highs._1=>
        println(rank.value * highs._2)
        println(s"rank: ${rank.value} high: $highs  low: $lows")
        rank.value * highs._2
      case (rank, listOfRank) if listOfRank.size == lows._1 =>
        println(rank.value * lows._2)
        println(s"rank: ${rank.value} high: $highs  low: $lows")
        rank.value * lows._2
      case (rank, listOfRank) if listOfRank.size == 1 =>
        rank.value
      case (_, _) => 0
    }.sum
  }

  def getTwoPairsScore(hand: Hand, highBase: Int, lowBase: Int) = {
    val sortedGroupedRankValues = toRankValues(hand).reverse.groupBy(identity)
    val sortedGroupedPairsRankValues = sortedGroupedRankValues.filter(_._2.size == 2)
    val highPairRank = sortedGroupedPairsRankValues.head._1
    val lowPairRank = sortedGroupedPairsRankValues.tail.head._1
    val singleCardRank = sortedGroupedRankValues.filter(_._2.size == 1).head._1
    highPairRank * highBase + lowPairRank * lowBase + singleCardRank
  }

  def getHighsValue(hand: Hand, highs: Int) = {
    toRankValues(hand).reverse.groupBy(identity).filter (_._2.size == highs).head._1
  }

  def isRoyalFlush(hand: Hand) = {
    val ranks = toRanks(hand)
    isStraightFlush(hand) && ranks.contains(Rank.Ace()) && ranks.contains(Rank.King)
  }

  def isStraightFlush(hand: Hand) = isFlush(hand) && isStraight(hand)

  def isFourOfAKind(hand: Hand) = toRankSizes(hand).contains(4)

  def isFullHouse(hand: Hand): Boolean = {
    val rankSizes = toRankSizes(hand)
    hand.size == 5 && rankSizes.contains(3) && rankSizes.contains(2)
  }

  def isFlush(hand: Hand): Boolean = hand.size == 5 && toSuits(hand).forall(_ == hand.head.suit)

  def isStraight(hand: Hand): Boolean = {
    val rankValues = toRankValues(hand)
    val minRank = rankValues.min
    hand.size == 5 && (rankValues.toSet == minRank.to(minRank + 4).toSet ||
      rankValues.sorted == (2 to 5).toList ++ List(14)) // corner case: Straight from Ace to Five
  }

  def isThreeOfAKind(hand: Hand): Boolean = toRankSizes(hand).contains(3)

  def isTwoPairs(hand: Hand): Boolean = {
    hand.size == 5 && toRankSizes(hand).count(_ == 2) == 2
  }

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
  def evaluateScore: Int
}

object HandType {
  case class RoyalFlush(row: Position)
    extends HandType(name = "RoyalFlush", strength = 10, position = row) {
    override def royalties: Int = row match {
      case Bottom => 25
      case Middle => 50
      case Top => throw new RuntimeException("Error: cannot have RoyalFlush in Top Position")
    }
    override def evaluateScore: Int = 10 + 11 + 12 + 13 + 14
  }
  case class StraightFlush(hand: Hand, row: Position)
    extends HandType(name = "StraightFlush", strength = 9, position = row) {
    override def royalties: Int = row match {
      case Bottom => 15
      case Middle => 30
      case Top => throw new RuntimeException("Error: cannot have StraightFlush in Top Position")
    }

    override def evaluateScore: Int =
      toRankValues(hand).sum
  }
  case class FourOfAKind(hand: Hand, row: Position)
    extends HandType(name = "FourOfAKind", strength = 8, position = row) {
    override def royalties: Int = row match {
      case Bottom => 10
      case Middle => 20
      case Top => throw new RuntimeException("Error: cannot have FourOfAKind in Top Position")
    }

    override def evaluateScore: Int =
      getHighsLows(hand, highs = (4, 100), lows = (1, 1))
  }
  case class FullHouse(hand: Hand, row: Position)
    extends HandType(name = "FullHouse", strength = 7, position = row) {
    override def royalties: Int = row match {
      case Bottom => 6
      case Middle => 12
      case Top => throw new RuntimeException("Error: cannot have FullHouse in Top Position")
    }

    override def evaluateScore: Int =
      getHighsLows(hand, highs = (3, 1000), lows = (2, 10))
  }
  case class Flush(hand: Hand, row: Position)
    extends HandType(name = "Flush", strength = 6, position = row) {
    override def royalties: Int = row match {
      case Bottom => 4
      case Middle => 8
      case Top => throw new RuntimeException("Error: cannot have Flush in Top Position")
    }

    override def evaluateScore: Int =
      toRankValues(hand).sum
  }
  case class Straight(hand: Hand, row: Position)
    extends HandType(name = "Straight", strength = 5, position = row) {
    override def royalties: Int = row match {
      case Bottom => 2
      case Middle => 4
      case Top => throw new RuntimeException("Error: cannot have Straight in Top Position")
    }

    override def evaluateScore: Int =
      toRankValues(hand).sum
  }
  case class ThreeOfAKind(hand: Hand, row: Position)
    extends HandType(name = "ThreeOfAKind", strength = 4, position = row) {
    override def royalties: Int = row match {
      case Bottom => 0
      case Middle => 2
      case Top => getHighsValue(hand, 3) + 8
    }

    override def evaluateScore: Int =
      getHighsLows(hand, highs = (3, 100), lows = (1, 1))
  }
  case class TwoPairs(hand: Hand, row: Position)
    extends HandType(name = "TwoPairs", strength = 3, position = row) {
    override def royalties: Int = 0
    override def evaluateScore: Int = getTwoPairsScore(hand, 400, 50)
  }
  case class SinglePair(hand: Hand, row: Position)
    extends HandType(name = "SinglePair", strength = 2, position = row) {
    override def royalties: Int = row match {
      case Top if getHighsValue(hand, 2) > 5 => getHighsValue(hand, 2) - 5
      case _ => 0
    }
    override def evaluateScore: Int =
      getHighsLows(hand, (2, 100), (1, 1))
  }
  case class HighCards(hand: Hand, row: Position) extends
    HandType(name = "HighCard", strength = 1, position = row) {
    override def royalties: Int = 0
    override def evaluateScore: Int =
      toRankValues(hand).sum
  }
}