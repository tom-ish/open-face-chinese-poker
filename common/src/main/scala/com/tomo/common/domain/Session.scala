package main.scala.com.tomo.common.domain

import akka.actor.ActorRef

/*
  Player's Session representation
 */
case class PlayerSession(player: Player, ref: ActorRef)

/*
  Game Session representation
 */
case class GameSession(room: GameRoom, ref: ActorRef, players: List[PlayerSession])