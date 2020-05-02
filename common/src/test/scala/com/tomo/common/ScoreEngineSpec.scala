package com.tomo.common

import main.scala.com.tomo.common.domain.{Bottom, Card, CardStack, Hand, Middle, Rank, Suit}
import org.scalatest.flatspec.AnyFlatSpec

class ScoreEngineSpec extends AnyFlatSpec {

  "A Score Engine (between two StraightFlush)" should "consider first hand stronger than second" in {
    val firstHand = Hand(CardStack(List(
      Card(Rank.Three(), Suit.Clubs()),
      Card(Rank.Two(), Suit.Clubs()),
      Card(Rank.Five(), Suit.Clubs()),
      Card(Rank.Ace(), Suit.Clubs()),
      Card(Rank.Four(), Suit.Clubs()))))
    val secondHand = Hand(CardStack(List(
      Card(Rank.Nine(), Suit.Diamonds()),
      Card(Rank.Five(), Suit.Diamonds()),
      Card(Rank.Seven(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Diamonds()),
      Card(Rank.Eight(), Suit.Diamonds()))))

    // WHEN POSITION = BOTTOM
    val lowerStraightFlushBottom = Hand.evaluate(firstHand, Bottom)
    val higherStraightFlushBottom = Hand.evaluate(secondHand, Bottom)
    assert(lowerStraightFlushBottom.name == "StraightFlush")
    assert(higherStraightFlushBottom.name == "StraightFlush")
    assert(lowerStraightFlushBottom.royalties == higherStraightFlushBottom.royalties)
    assert(lowerStraightFlushBottom.evaluateScore < higherStraightFlushBottom.evaluateScore)

    //WHEN POSITION == MIDDLE
    val lowerStraightFlushMiddle = Hand.evaluate(firstHand, Middle)
    val higherStraightFlushMiddle = Hand.evaluate(secondHand, Middle)
    assert(lowerStraightFlushMiddle.name == "StraightFlush")
    assert(higherStraightFlushMiddle.name == "StraightFlush")
    assert(lowerStraightFlushMiddle.royalties == higherStraightFlushMiddle.royalties)
    assert(lowerStraightFlushMiddle.evaluateScore < higherStraightFlushMiddle.evaluateScore)
  }

  "A Score Engine (between two Flush)" should "consider first hand stronger than second" in {
    val firstHand = Hand(CardStack(List(
      Card(Rank.Ten(), Suit.Clubs()),
      Card(Rank.Seven(), Suit.Clubs()),
      Card(Rank.Five(), Suit.Clubs()),
      Card(Rank.Two(), Suit.Clubs()),
      Card(Rank.Eight(), Suit.Clubs()))))
    val secondHand = Hand(CardStack(List(
      Card(Rank.Nine(), Suit.Diamonds()),
      Card(Rank.Two(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Diamonds()),
      Card(Rank.Queen(), Suit.Diamonds()),
      Card(Rank.Eight(), Suit.Diamonds()))))

    // WHEN POSITION = BOTTOM
    val lowerFlushBottom = Hand.evaluate(firstHand, Bottom)
    val higherFlushBottom = Hand.evaluate(secondHand, Bottom)
    assert(lowerFlushBottom.name == "Flush")
    assert(higherFlushBottom.name == "Flush")
    assert(lowerFlushBottom.royalties == higherFlushBottom.royalties)
    assert(lowerFlushBottom.evaluateScore < higherFlushBottom.evaluateScore)

    //WHEN POSITION == MIDDLE
    val lowerFlushMiddle = Hand.evaluate(firstHand, Middle)
    val higherFlushMiddle = Hand.evaluate(secondHand, Middle)
    assert(lowerFlushMiddle.name == "Flush")
    assert(higherFlushMiddle.name == "Flush")
    assert(lowerFlushMiddle.royalties == higherFlushMiddle.royalties)
    assert(lowerFlushMiddle.evaluateScore < higherFlushMiddle.evaluateScore)
  }

  "A Score Engine (between two Straight)" should "consider first hand stronger than second" in {
    val firstHand = Hand(CardStack(List(
      Card(Rank.Nine(), Suit.Hearts()),
      Card(Rank.Eight(), Suit.Clubs()),
      Card(Rank.Ten(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Spades()),
      Card(Rank.Seven(), Suit.Hearts()))))
    val secondHand = Hand(CardStack(List(
      Card(Rank.Six(), Suit.Diamonds()),
      Card(Rank.Two(), Suit.Clubs()),
      Card(Rank.Four(), Suit.Hearts()),
      Card(Rank.Five(), Suit.Spades()),
      Card(Rank.Three(), Suit.Clubs()))))

    // WHEN POSITION = BOTTOM
    val lowerStraightBottom = Hand.evaluate(firstHand, Bottom)
    val higherStraightBottom = Hand.evaluate(secondHand, Bottom)
    assert(lowerStraightBottom.name == "Straight")
    assert(higherStraightBottom.name == "Straight")
    assert(lowerStraightBottom.royalties == higherStraightBottom.royalties)
    assert(lowerStraightBottom.evaluateScore < higherStraightBottom.evaluateScore)

    //WHEN POSITION == MIDDLE
    val lowerStraightMiddle = Hand.evaluate(firstHand, Middle)
    val higherStraightMiddle = Hand.evaluate(secondHand, Middle)
    assert(lowerStraightMiddle.name == "Straight")
    assert(higherStraightMiddle.name == "Straight")
    assert(lowerStraightMiddle.royalties == higherStraightMiddle.royalties)
    assert(lowerStraightMiddle.evaluateScore < higherStraightMiddle.evaluateScore)
  }

  "A Score Engine (between two ThreeOfAKind)" should "consider first hand stronger than second" in {
    val firstHand = Hand(CardStack(List(
      Card(Rank.Six(), Suit.Hearts()),
      Card(Rank.Eight(), Suit.Clubs()),
      Card(Rank.Six(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Spades()),
      Card(Rank.Seven(), Suit.Hearts()))))
    val secondHand = Hand(CardStack(List(
      Card(Rank.Nine(), Suit.Diamonds()),
      Card(Rank.Two(), Suit.Clubs()),
      Card(Rank.Nine(), Suit.Hearts()),
      Card(Rank.Nine(), Suit.Spades()),
      Card(Rank.Six(), Suit.Clubs()))))

    // WHEN POSITION = BOTTOM
    val lowerThreeOfAKindBottom = Hand.evaluate(firstHand, Bottom)
    val higherThreeOfAKindBottom = Hand.evaluate(secondHand, Bottom)
    assert(lowerThreeOfAKindBottom.name == "ThreeOfAKind")
    assert(higherThreeOfAKindBottom.name == "ThreeOfAKind")
    assert(lowerThreeOfAKindBottom.royalties == higherThreeOfAKindBottom.royalties)
    assert(lowerThreeOfAKindBottom.evaluateScore < higherThreeOfAKindBottom.evaluateScore)

    //WHEN POSITION == MIDDLE
    val lowerThreeOfAKindMiddle = Hand.evaluate(firstHand, Middle)
    val higherThreeOfAKindMiddle = Hand.evaluate(secondHand, Middle)
    assert(lowerThreeOfAKindMiddle.name == "ThreeOfAKind")
    assert(higherThreeOfAKindMiddle.name == "ThreeOfAKind")
    assert(lowerThreeOfAKindMiddle.royalties == higherThreeOfAKindMiddle.royalties)
    assert(lowerThreeOfAKindMiddle.evaluateScore < higherThreeOfAKindMiddle.evaluateScore)
  }

  "A Score Engine (between two TwoPairs)" should "consider first hand stronger than second" in {
    val firstHand = Hand(CardStack(List(
      Card(Rank.Six(), Suit.Hearts()),
      Card(Rank.Eight(), Suit.Clubs()),
      Card(Rank.Nine(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Spades()),
      Card(Rank.Nine(), Suit.Hearts()))))
    val secondHand = Hand(CardStack(List(
      Card(Rank.Nine(), Suit.Spades()),
      Card(Rank.Two(), Suit.Clubs()),
      Card(Rank.Nine(), Suit.Clubs()),
      Card(Rank.Two(), Suit.Spades()),
      Card(Rank.Six(), Suit.Clubs()))))

    // WHEN POSITION = BOTTOM
    val lowerTwoPairsBottom = Hand.evaluate(firstHand, Bottom)
    val higherTwoPairsBottom = Hand.evaluate(secondHand, Bottom)
    assert(lowerTwoPairsBottom.name == "TwoPairs")
    assert(higherTwoPairsBottom.name == "TwoPairs")
    assert(lowerTwoPairsBottom.royalties == higherTwoPairsBottom.royalties)
    assert(lowerTwoPairsBottom.evaluateScore < higherTwoPairsBottom.evaluateScore)

    //WHEN POSITION == MIDDLE
    val lowerTwoPairsMiddle = Hand.evaluate(firstHand, Middle)
    val higherTwoPairsMiddle = Hand.evaluate(secondHand, Middle)
    assert(lowerTwoPairsMiddle.name == "TwoPairs")
    assert(higherTwoPairsMiddle.name == "TwoPairs")
    assert(lowerTwoPairsMiddle.royalties == higherTwoPairsMiddle.royalties)
    assert(lowerTwoPairsMiddle.evaluateScore < higherTwoPairsMiddle.evaluateScore)
  }

  "A Score Engine (between two SinglePairs)" should "consider first hand stronger than second" in {
    val firstHand = Hand(CardStack(List(
      Card(Rank.Six(), Suit.Hearts()),
      Card(Rank.Eight(), Suit.Clubs()),
      Card(Rank.Ace(), Suit.Diamonds()),
      Card(Rank.Two(), Suit.Spades()),
      Card(Rank.Six(), Suit.Hearts()))))
    val secondHand = Hand(CardStack(List(
      Card(Rank.Nine(), Suit.Spades()),
      Card(Rank.Two(), Suit.Diamonds()),
      Card(Rank.Nine(), Suit.Clubs()),
      Card(Rank.Ace(), Suit.Spades()),
      Card(Rank.Six(), Suit.Clubs()))))

    // WHEN POSITION = BOTTOM
    val lowerSinglePairsBottom = Hand.evaluate(firstHand, Bottom)
    val higherSinglePairsBottom = Hand.evaluate(secondHand, Bottom)
    assert(lowerSinglePairsBottom.name == "SinglePair")
    assert(higherSinglePairsBottom.name == "SinglePair")
    assert(lowerSinglePairsBottom.royalties == higherSinglePairsBottom.royalties)
    assert(lowerSinglePairsBottom.evaluateScore < higherSinglePairsBottom.evaluateScore)

    //WHEN POSITION == MIDDLE
    val lowerSinglePairsMiddle = Hand.evaluate(firstHand, Middle)
    val higherSinglePairsMiddle = Hand.evaluate(secondHand, Middle)
    assert(lowerSinglePairsMiddle.name == "SinglePair")
    assert(higherSinglePairsMiddle.name == "SinglePair")
    assert(lowerSinglePairsMiddle.royalties == higherSinglePairsMiddle.royalties)
    assert(lowerSinglePairsMiddle.evaluateScore < higherSinglePairsMiddle.evaluateScore)
  }

  
}