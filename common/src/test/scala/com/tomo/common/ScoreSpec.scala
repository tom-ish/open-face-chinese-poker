package com.tomo.common

import main.scala.com.tomo.common.domain._
import org.scalatest.flatspec.AnyFlatSpec

class ScoreSpec extends AnyFlatSpec {

  "A Score Engine (when position=Bottom)" should "evaluate a StraightFlush" in {
    val straightFlush = Hand(CardStack(List(
      Card(Rank.Four(), Suit.Hearts()),
      Card(Rank.Two(), Suit.Hearts()),
      Card(Rank.Ace(), Suit.Hearts()),
      Card(Rank.Three(), Suit.Hearts()),
      Card(Rank.Five(), Suit.Hearts()))))

    val straightFlushHand = Hand.evaluate(straightFlush, Bottom)
    assert(straightFlushHand.strength == 9)
    assert(straightFlushHand.name == "StraightFlush")
    assert(straightFlushHand.evaluateScore == 28)
    assert(straightFlushHand.royalties == 15)
  }

  it should "evaluate a FourOfAKind" in {
    val fourOfAKind = Hand(CardStack(List(
      Card(Rank.Four(), Suit.Clubs()),
      Card(Rank.Ace(), Suit.Clubs()),
      Card(Rank.Four(), Suit.Diamonds()),
      Card(Rank.Four(), Suit.Hearts()),
      Card(Rank.Four(), Suit.Spades())
    )))

    val fourOfAKindHand = Hand.evaluate(fourOfAKind, Bottom)
    assert(fourOfAKindHand.strength == 8)
    assert(fourOfAKindHand.name == "FourOfAKind")
    assert(fourOfAKindHand.evaluateScore == 414)
    assert(fourOfAKindHand.royalties == 10)
  }

  it should "evaluate a Full House" in {
    val fullHouse = Hand(CardStack(List(
      Card(Rank.Ten(), Suit.Clubs()),
      Card(Rank.King(), Suit.Hearts()),
      Card(Rank.Ten(), Suit.Diamonds()),
      Card(Rank.King(), Suit.Spades()),
      Card(Rank.King(), Suit.Clubs()),
    )))

    val fullHouseHand = Hand.evaluate(fullHouse, Bottom)
    assert(fullHouseHand.strength == 7)
    assert(fullHouseHand.name == "FullHouse")
    assert(fullHouseHand.evaluateScore == 13000 + 100)
    assert(fullHouseHand.royalties == 6)
  }

  it should "evaluate a Flush" in {
    val flush = Hand(CardStack(List(
      Card(Rank.Five(), Suit.Hearts()),
      Card(Rank.Eight(), Suit.Hearts()),
      Card(Rank.Ten(), Suit.Hearts()),
      Card(Rank.Six(), Suit.Hearts()),
      Card(Rank.Queen(), Suit.Hearts()))))

    val flushHand = Hand.evaluate(flush, Bottom)
    assert(flushHand.strength == 6)
    assert(flushHand.name == "Flush")
    assert(flushHand.evaluateScore == 5+8+10+6+12)
    assert(flushHand.royalties == 4)
  }

  it should "evaluate a Straight" in {
    val straight = Hand(CardStack(List(
      Card(Rank.Nine(), Suit.Hearts()),
      Card(Rank.Eight(), Suit.Clubs()),
      Card(Rank.Ten(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Spades()),
      Card(Rank.Seven(), Suit.Hearts()))))

    val straightHand = Hand.evaluate(straight, Bottom)
    assert(straightHand.strength == 5)
    assert(straightHand.name == "Straight")
    assert(straightHand.evaluateScore == 6+7+8+9+10)
    assert(straightHand.royalties == 2)
  }

  it should "evaluate a ThreeOfAKind" in {
    val threeOfJacks = Hand(CardStack(List(
      Card(Rank.Five(), Suit.Hearts()),
      Card(Rank.Jack(), Suit.Diamonds()),
      Card(Rank.Ten(), Suit.Hearts()),
      Card(Rank.Jack(), Suit.Clubs()),
      Card(Rank.Jack(), Suit.Hearts()))))

    val threeOfAKindHand = Hand.evaluate(threeOfJacks, Bottom)
    assert(threeOfAKindHand.strength == 4)
    assert(threeOfAKindHand.name == "ThreeOfAKind")
    assert(threeOfAKindHand.evaluateScore == 100*11 + (5+10))
    assert(threeOfAKindHand.royalties == 0)
  }

  it should "evaluate a TwoPairs" in {
    val twoPairs = Hand(CardStack(List(
      Card(Rank.Two(), Suit.Spades()),
      Card(Rank.Six(), Suit.Clubs()),
      Card(Rank.Ten(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Diamonds()),
      Card(Rank.Two(), Suit.Hearts())
    )))

    val twoPairsHand = Hand.evaluate(twoPairs, Bottom)
    assert(twoPairsHand.strength == 3)
    assert(twoPairsHand.name == "TwoPairs")
    assert(twoPairsHand.evaluateScore == 400*6 + 50*2 + 10)
    assert(twoPairsHand.royalties == 0)
  }

  it should "evaluate a SinglePair" in {
    val singlePair = Hand(CardStack(List(
      Card(Rank.Five(), Suit.Spades()),
      Card(Rank.Ace(), Suit.Diamonds()),
      Card(Rank.Ten(), Suit.Hearts()),
      Card(Rank.Ace(), Suit.Hearts()),
      Card(Rank.Jack(), Suit.Spades()),
    )))

    val singlePairHand = Hand.evaluate(singlePair, Bottom)
    assert(singlePairHand.strength == 2)
    assert(singlePairHand.name == "SinglePair")
    assert(singlePairHand.evaluateScore == 1400 + 5+10+11)
    assert(singlePairHand.royalties == 0)
  }

  "A Score Engine (when position=middle)" should "evaluate a StraightFlush" in {
    val straightFlush = Hand(CardStack(List(
      Card(Rank.Four(), Suit.Hearts()),
      Card(Rank.Two(), Suit.Hearts()),
      Card(Rank.Ace(), Suit.Hearts()),
      Card(Rank.Three(), Suit.Hearts()),
      Card(Rank.Five(), Suit.Hearts()))))

    val straightFlushHand = Hand.evaluate(straightFlush, Middle)
    assert(straightFlushHand.strength == 9)
    assert(straightFlushHand.name == "StraightFlush")
    assert(straightFlushHand.evaluateScore == 28)
    assert(straightFlushHand.royalties == 30)
  }

  it should "evaluate a FourOfAKind" in {
    val fourOfAKind = Hand(CardStack(List(
      Card(Rank.Four(), Suit.Clubs()),
      Card(Rank.Ace(), Suit.Clubs()),
      Card(Rank.Four(), Suit.Diamonds()),
      Card(Rank.Four(), Suit.Hearts()),
      Card(Rank.Four(), Suit.Spades())
    )))

    val fourOfAKindHand = Hand.evaluate(fourOfAKind, Middle)
    assert(fourOfAKindHand.strength == 8)
    assert(fourOfAKindHand.name == "FourOfAKind")
    assert(fourOfAKindHand.evaluateScore == 414)
    assert(fourOfAKindHand.royalties == 20)
  }

  it should "evaluate a Full House" in {
    val fullHouse = Hand(CardStack(List(
      Card(Rank.Ten(), Suit.Clubs()),
      Card(Rank.King(), Suit.Hearts()),
      Card(Rank.Ten(), Suit.Diamonds()),
      Card(Rank.King(), Suit.Spades()),
      Card(Rank.King(), Suit.Clubs()),
    )))

    val fullHouseHand = Hand.evaluate(fullHouse, Middle)
    assert(fullHouseHand.strength == 7)
    assert(fullHouseHand.name == "FullHouse")
    assert(fullHouseHand.evaluateScore == 13000 + 100)
    assert(fullHouseHand.royalties == 12)
  }

  it should "evaluate a Flush" in {
    val flush = Hand(CardStack(List(
      Card(Rank.Five(), Suit.Hearts()),
      Card(Rank.Eight(), Suit.Hearts()),
      Card(Rank.Ten(), Suit.Hearts()),
      Card(Rank.Six(), Suit.Hearts()),
      Card(Rank.Queen(), Suit.Hearts()))))

    val flushHand = Hand.evaluate(flush, Middle)
    assert(flushHand.strength == 6)
    assert(flushHand.name == "Flush")
    assert(flushHand.evaluateScore == 5+8+10+6+12)
    assert(flushHand.royalties == 8)
  }

  it should "evaluate a Straight" in {
    val straight = Hand(CardStack(List(
      Card(Rank.Nine(), Suit.Hearts()),
      Card(Rank.Eight(), Suit.Clubs()),
      Card(Rank.Ten(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Spades()),
      Card(Rank.Seven(), Suit.Hearts()))))

    val straightHand = Hand.evaluate(straight, Middle)
    assert(straightHand.strength == 5)
    assert(straightHand.name == "Straight")
    assert(straightHand.evaluateScore == 6+7+8+9+10)
    assert(straightHand.royalties == 4)
  }

  it should "evaluate a ThreeOfAKind" in {
    val threeOfJacks = Hand(CardStack(List(
      Card(Rank.Five(), Suit.Hearts()),
      Card(Rank.Jack(), Suit.Diamonds()),
      Card(Rank.Ten(), Suit.Hearts()),
      Card(Rank.Jack(), Suit.Clubs()),
      Card(Rank.Jack(), Suit.Hearts()))))

    val threeOfAKindHand = Hand.evaluate(threeOfJacks, Middle)
    assert(threeOfAKindHand.strength == 4)
    assert(threeOfAKindHand.name == "ThreeOfAKind")
    assert(threeOfAKindHand.evaluateScore == 100*11 + (5+10))
    assert(threeOfAKindHand.royalties == 2)
  }

  it should "evaluate a TwoPairs" in {
    val twoPairs = Hand(CardStack(List(
      Card(Rank.Two(), Suit.Spades()),
      Card(Rank.Six(), Suit.Clubs()),
      Card(Rank.Ten(), Suit.Diamonds()),
      Card(Rank.Six(), Suit.Diamonds()),
      Card(Rank.Two(), Suit.Hearts())
    )))

    val twoPairsHand = Hand.evaluate(twoPairs, Middle)
    assert(twoPairsHand.strength == 3)
    assert(twoPairsHand.name == "TwoPairs")
    assert(twoPairsHand.evaluateScore == 400*6 + 50*2 + 10)
    assert(twoPairsHand.royalties == 0)
  }

  it should "evaluate a SinglePair" in {
    val singlePair = Hand(CardStack(List(
      Card(Rank.Five(), Suit.Spades()),
      Card(Rank.Ace(), Suit.Diamonds()),
      Card(Rank.Ten(), Suit.Hearts()),
      Card(Rank.Ace(), Suit.Hearts()),
      Card(Rank.Jack(), Suit.Spades()),
    )))

    val singlePairHand = Hand.evaluate(singlePair, Middle)
    assert(singlePairHand.strength == 2)
    assert(singlePairHand.name == "SinglePair")
    assert(singlePairHand.evaluateScore == 1400 + 5+10+11)
    assert(singlePairHand.royalties == 0)
  }

  "A Score Engine (when position=Top)"  should "evaluate a ThreeOfAKind" in {
    val threeOfJacks = Hand(CardStack(List(
      Card(Rank.Jack(), Suit.Diamonds()),
      Card(Rank.Jack(), Suit.Clubs()),
      Card(Rank.Jack(), Suit.Hearts()))))

    val threeOfAKindHand = Hand.evaluate(threeOfJacks, Top)
    assert(threeOfAKindHand.strength == 4)
    assert(threeOfAKindHand.name == "ThreeOfAKind")
    assert(threeOfAKindHand.evaluateScore == 100*11)
    assert(threeOfAKindHand.royalties == 19)
  }

  it should "evaluate a SinglePair" in {
    val singlePair = Hand(CardStack(List(
      Card(Rank.Five(), Suit.Spades()),
      Card(Rank.Ace(), Suit.Diamonds()),
      Card(Rank.Ace(), Suit.Hearts())
    )))

    val singlePairHand = Hand.evaluate(singlePair, Top)
    assert(singlePairHand.strength == 2)
    assert(singlePairHand.name == "SinglePair")
    assert(singlePairHand.evaluateScore == 1405)
    assert(singlePairHand.royalties == 9)
  }
}
