package com.tomo.game.domain

case class Player(id: String, name: String)

case class PlayerScore(totalPoints: Int)

object PlayerScore {
  val zero = PlayerScore(0)
}

case class PlayerState(playerHand: PlayerHand)