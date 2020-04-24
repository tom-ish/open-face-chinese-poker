package com.tomo.game.server.actor

import akka.actor.{Actor, ActorLogging, Props}
import com.tomo.game.Messages
import com.tomo.game.domain.{Bottom, Card, CardStack, Combination, Middle, PlayerDeck, Position, Top}
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
      // Check if the PlayerDeck is valid <=> the order of strength does not go top to bottom
      val validPlayers: Map[PlayerSession, PlayerDeck] = playersDecks.filter(playerDeck => PlayerDeck.isValid(playerDeck._2.deck))
      val invalidPlayers = playersDecks.toSeq.diff(validPlayers.toSeq)

      /**
       * For each player that has a valid deck:
       *  - first: build a Combination object from players deck
       *  - second: compute the score of each deck row
       *  - then: compare each row with all players' corresponding deck's row
       */

      // Transform each row to Combination //TODO Combination build logic
      val scoresPerPlayer = validPlayers.map { // for each player
        case (playerSession, playerDeck) =>
          val playerScore = playerDeck.deck map {  // for each player deck cards row
            case (position, cards) => (position, Combination.transform(cards).score)
          }
          (playerSession, playerScore)
      }


      val b = for {
        scorePerRow <- scoresPerPlayer
        (pPosition, playerScore) <- scorePerRow._2
        opponentScorePerRow <- scoresPerPlayer.filterNot(_._1.ref == scorePerRow._1.ref)
        (oPosition, opponentScore) <- opponentScorePerRow._2
      } yield (pPosition, playerScore)zip (oPosition, opponentScore)

      b


        val battle = for {
          player <- playerScores
          opponent <- playerScores.filterNot(player == _)
        } yield player._2 zip opponent._2

        val r = battle map(scorePairPerRow => scorePairPerRow.map {
          case (rowScore, rowScoreOpponent) => rowScore - rowScoreOpponent
        })
      }



      playerScores.foreach {
        case (session, scores) =>

      }



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
