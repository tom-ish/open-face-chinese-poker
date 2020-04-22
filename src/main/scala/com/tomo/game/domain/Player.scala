package com.tomo.game.domain

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
  def empty = PlayerDeck(deck = Map.empty)
  implicit def map2deck(deck: Map[Position, CardStack]) = PlayerDeck(deck)
}
case class PlayerDeck(deck: Map[Position, CardStack]) {
  def merge(cards: PlayerDeck): PlayerDeck = PlayerDeck(deck ++ cards.deck)

  def isValid: Boolean =

  override def toString: String = deck.mkString(" | ")
}

abstract class Position(val nbCards: Int)
case object Top extends Position(nbCards = 3)
case object Middle extends Position(nbCards = 5)
case object Bottom extends Position(nbCards = 5)