package com.tomo.server.actor

import akka.actor.{Actor, ActorLogging, Props}
import main.scala.com.tomo.common.Messages
import main.scala.com.tomo.common.domain.{Card, PlayerDeck, PlayerSession}

object ScoreEngineActor {
  def props(finalDeck: Map[PlayerSession, PlayerDeck]) = Props(new ScoreEngineActor(finalDeck))
}

class ScoreEngineActor(val playersDecks: Map[PlayerSession, PlayerDeck]) extends Actor with ActorLogging {
  override def receive: Receive = computeScore

  def computeScore: Receive = {
    case Messages.Score.GetBoardPointWinner(playersPairScores) =>
      val scoresPerPosition = playersPairScores map {
        case (p1ScoresRoyalties, p2ScoresRoyalties) =>
          p1ScoresRoyalties.zip(p2ScoresRoyalties)
            .map { scoreRoyaltiesPerPosition =>
              val diff = scoreRoyaltiesPerPosition._1._2._1 - scoreRoyaltiesPerPosition._2._2._1
              if(diff > 0)
                (Some(scoreRoyaltiesPerPosition._1._1), (Some(scoreRoyaltiesPerPosition._1._1), Some(scoreRoyaltiesPerPosition._2._1)))
              else if(diff < 0)
                (Some(scoreRoyaltiesPerPosition._2._1), (Some(scoreRoyaltiesPerPosition._1._1), Some(scoreRoyaltiesPerPosition._2._1)))
              else
                (None, (Some(scoreRoyaltiesPerPosition._1._1), Some(scoreRoyaltiesPerPosition._2._1)))
            }
      }

      val winnerBoardPoint = scoresPerPosition map {
        case Seq((None, (p1Top,p2Top)), (None, (_,_)), (None, (_,_))) =>
          Messages.Score.BoardPointWinner(None, (p1Top.get, p2Top.get))
        case Seq((w1, (p1Top,p2Top)), (None, (_,_)), (None, (_,_))) =>
          Messages.Score.BoardPointWinner(w1, (p1Top.get,p2Top.get))
        case Seq((None, (p1Top,p2Top)), (w1,(_,_)), (None, (_,_))) =>
          Messages.Score.BoardPointWinner(w1, (p1Top.get,p2Top.get))
        case Seq((None, (p1Top,p2Top)), (None, (_,_)), (w1, (_,_))) =>
          Messages.Score.BoardPointWinner(w1, (p1Top.get,p2Top.get))
        case Seq((None, (p1Top,p2Top)), (w2, (_,_)), (w3, (_,_))) =>
          if (w2.get == w3.get) Messages.Score.BoardPointWinner(w2, (p1Top.get,p2Top.get))
          else Messages.Score.BoardPointWinner(None, (p1Top.get, p2Top.get))
        case Seq((w1, (p1Top,p2Top)), (None, (_,_)), (w2, (_,_))) =>
          if(w1.get == w2.get) Messages.Score.BoardPointWinner(w1, (p1Top.get,p2Top.get))
          else Messages.Score.BoardPointWinner(None, (p1Top.get, p2Top.get))
        case Seq((w1, (p1Top,p2Top)), (w2, (_,_)), (None, (_,_))) =>
          if(w1.get == w2.get) Messages.Score.BoardPointWinner(w1, (p1Top.get,p2Top.get))
          else Messages.Score.BoardPointWinner(None, (p1Top.get, p2Top.get))
      }
      sender ! winnerBoardPoint

    case Messages.Score.ComputeRoyalties(playersPairRoyalties) =>
      if(playersPairRoyalties.size == 2) {
        val royaltiesPerPosition = playersPairRoyalties map {
          case (p1Royalties, p2Royalties) =>
            val diff = p1Royalties._2 - p2Royalties._2
            if(diff > 0)
              (p1Royalties._1, diff)
            else if(diff < 0)
              (p1Royalties._1, diff)
        }
      } else if(playersPairRoyalties.size == 3) {

      } else throw new RuntimeException("Error in ComputeRoyalties: There are more than 2 or 3 players")

/*

      val royaltiesPerPosition = playersPairRoyalties map {
        case (p1Royalties, p2Royalties) =>
          val diff = p1Royalties._2 - p2Royalties._2

      }

      roya

      val rslt: SortedMap[Int, PlayerSession] = SortedMap()
      //TODO compute the score for all players <=> engine for 2 as well as 3 players
      // Check if the PlayerDeck is valid <=> the order of strength does not go top to bottom

      for {
        (playerSession, playerDeck) <- playersDecks
        (position, cardStack) <- playerDeck.deck
      }

      val playersValid = playersDecks.map {
        case (playerSession, playerDeck) => (playerSession, Hand.evaluate(playerDeck.deck))
      }


      val validPlayers: Map[PlayerSession, PlayerDeck] = playersDecks.filter(playerDeck => PlayerDeck.isValid(playerDeck._2.deck))
      val invalidPlayers = playersDecks.toSeq.diff(validPlayers.toSeq)

      val playerPair = for {
        player1 <- validPlayers
        player2 <- validPlayers
      } yield (player1, player2)

      player

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
            case (position, cards) => (position, HandType.transform(cards, position).score)
          }
          (playerSession, playerScore)
      }


      val b = for {
        scorePerRow <- scoresPerPlayer
        (pPosition, playerScore) <- scorePerRow._2
        opponentScorePerRow <- scoresPerPlayer.filterNot(_._1.ref == scorePerRow._1.ref)
        (oPosition, opponentScore) <- opponentScorePerRow._2
      } yield List((pPosition, playerScore)) zip List((oPosition, opponentScore))
      b
*/

/*

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
*/

  }

}
