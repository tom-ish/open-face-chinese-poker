package com.tomo.game.server

import akka.actor.ActorRef
import com.tomo.game.domain.{GameRoom, Player}

/*
  Player's Session representation
 */
case class PlayerSession(player: Player, ref: ActorRef)

/*
  Game Session representation
 */
case class GameSession(room: GameRoom, ref: ActorRef, players: List[PlayerSession])