package com.tomo.game.domain

import akka.util.Collections

import scala.collection.SortedMap
import scala.collection.immutable.{AbstractSeq, LinearSeq}

case class Player(id: String, name: String)

case class PlayerScore(totalPoints: Int)

object PlayerScore {
  val zero = PlayerScore(0)
}


object PlayerHand {
  def empty = PlayerHand(
    hand                    = CardStack.empty,
    playerDeck              = Map.empty,
    dropStack               = CardStack.empty,
    playerNextMoveCardStack = CardStack.empty)
}
case class PlayerHand(hand: CardStack, playerDeck: Map[Position, CardStack], dropStack: CardStack, playerNextMoveCardStack: CardStack) {
  def add(card: Card, position: Position): PlayerHand = {
    playerDeck.foreachEntry { (p, cardsStack) =>
      if(p == position) {
        playerDeck += (position -> CardStack(cardsStack.cards :+ card))
      }
    }
    PlayerHand(hand, playerDeck, dropStack, CardStack(playerNextMoveCardStack.cards :+ card))
  }

  def remove(card: Card, position: Position) = {
    playerDeck.foreachEntry { (p, cardsStack) =>
      if(p == position)
        playerDeck += (position -> CardStack(cardsStack.cards.filterNot(_ == card)))
    }
    PlayerHand(hand, playerDeck, dropStack, CardStack(playerNextMoveCardStack.cards.filterNot(_ == card)))
  }

  def drop(card: Card): CardStack = CardStack(dropStack.cards :+ card)

  override def toString: String = playerDeck.mkString(" | ")
}

object PlayerDeck {
  def empty = PlayerDeck(deck = SortedMap.empty[Position, CardStack])

  def isValid(deck: SortedMap[Position, CardStack]) = {
    val sortedByPositionDeck = deck.toSeq.sortBy(_._1.position)
    sortedByPositionDeck.sliding(2).toArray.forall {
      case pairOfDeck =>
        Score(pairOfDeck.head._2).compute < Score(pairOfDeck.tail.head._2).compute
    }
  }

  implicit def map2deck(deck: SortedMap[Position, CardStack]) = PlayerDeck(deck)
}
case class PlayerDeck(deck: SortedMap[Position, CardStack]) {
  def merge(cards: PlayerDeck): PlayerDeck = PlayerDeck(deck ++ cards.deck)


  override def toString: String = deck.mkString(" | ")
}

abstract class Position(val position: Int, val nbCards: Int)
case object Top extends Position(position = 0, nbCards = 3)
case object Middle extends Position(position = 1, nbCards = 5)
case object Bottom extends Position(position = 2, nbCards = 5)