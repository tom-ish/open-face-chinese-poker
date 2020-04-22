package com.tomo.game.server.actor

import akka.actor.{Actor, ActorLogging, Props}
import com.tomo.game.domain.PlayerDeck
import com.tomo.game.server.PlayerSession
import com.tomo.game.server.actor.GameSupervisorActor.ComputeScore

import scala.collection.SortedMap

object ScoreEngineActor {
  def props(finalDeck: Map[PlayerSession, PlayerDeck]) = Props(new ScoreEngineActor(finalDeck))
}

class ScoreEngineActor(val playersDecks: Map[PlayerSession, PlayerDeck]) extends Actor with ActorLogging {
  override def receive: Receive = computeScore

  def computeScore: Receive = {
    case ComputeScore =>
      val rslt: SortedMap[Int, PlayerSession] = SortedMap()
      //TODO compute the score for all players <=> engine for 2 as well as 3 players

      sender ! rslt
  }
}
