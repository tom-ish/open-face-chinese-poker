package com.tomo.game.server.actor

import akka.actor.{Actor, ActorLogging, Props}
import com.tomo.game.Messages
import com.tomo.game.domain.{Bottom, Card, CardStack, Middle, PlayerDeck, Top}
import com.tomo.game.server.PlayerSession
import com.tomo.game.server.actor.GameSupervisorActor.ComputeScore

import scala.collection.SortedMap

object ScoreEngineActor {
  def props(finalDeck: Map[PlayerSession, PlayerDeck]) = Props(new ScoreEngineActor(finalDeck))
}

class ScoreEngineActor(val playersDecks: Map[PlayerSession, PlayerDeck]) extends Actor with ActorLogging {
  override def receive: Receive = computeScore

  def computeScore: Receive = {
    case Messages.Game.ComputeScore =>
      val rslt: SortedMap[Int, PlayerSession] = SortedMap()
      //TODO compute the score for all players <=> engine for 2 as well as 3 players

      playersDecks.foreachEntry { (playerSession, playerDeck) =>
        val positionScores = playerDeck.deck map {
          case (Top, cards) =>
            val score = cards.
          case (Middle, _) =>
          case (Bottom, _) =>

        }

      }

      sender ! rslt
  }

  private def computeScore(cards: List[Card]): Int = {
    cards.size match {
      case Top.nbCards =>
        cards.forall(_.rank == cards.head.rank)
      case Middle.nbCards || Bottom.nbCards =>

    }
  }
}
