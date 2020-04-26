package main.scala.com.tomo.game.server.actor

import akka.actor
import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Terminated}
import akka.util.Timeout
import com.tomo.server.actor.ScoreEngineActor
import main.scala.com.tomo.common.Messages
import main.scala.com.tomo.common.domain.{CardStack, FifthDraw, FirstDraw, GameRoom, Hand, Phase, PlayerDeck, PlayerSession}
import main.scala.com.tomo.game.server.actor.GameSupervisorActor.UnavailableRequest

import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

import akka.pattern.ask

object GameSupervisorActor {

  case class GameState(phase: Phase, deck: CardStack, visibleDeck: Map[PlayerSession, PlayerDeck], playerIterator: Iterator[(ActorRef, PlayerSession)])
  case class DrawState(phase: Phase, deck: CardStack, visibleDeck: Map[PlayerSession, PlayerDeck], nbCardsDistributed: Int, playerIterator: Iterator[(ActorRef, PlayerSession)])
  case object UnavailableRequest

  object Messages {
    case class ReceivePlayers(players: List[PlayerSession])
  }

  def props(gameRoom: GameRoom) = actor.Props(new GameSupervisorActor(gameRoom))
}

class GameSupervisorActor(val room: GameRoom) extends Actor with ActorLogging {
  implicit val timeout: Timeout = 5 seconds
  implicit val executionContext = context.dispatcher

  var players: mutable.LinkedHashMap[ActorRef, PlayerSession] = mutable.LinkedHashMap()

  def startGame(playersSessions: List[PlayerSession]) = {
    /* broadcast to all players that the other players joined the room */
    playersSessions.foreach(_.ref ! Messages.Game.Joined(room))

    val deck = CardStack.shuffled.cards

    val initialDrawState = GameSupervisorActor.DrawState(FirstDraw, deck, Map.empty, 0, players.iterator)
    context become distributing(initialDrawState)

    // INTRODUCE PLAYERS
    playersSessions.foreach(_.ref ! Messages.Game.SetUp)
  }

  def initializing: Receive = {
    case GameSupervisorActor.Messages.ReceivePlayers(playersList: List[PlayerSession]) => {
      playersList.foreach { p =>
        players += (p.ref -> p)
        context.watch(p.ref)
      }
      startGame(playersList)
    }
  }

  def distributing(drawState: GameSupervisorActor.DrawState): Receive = {
    case Messages.Game.DrawTime =>
      val phase = drawState.phase
      val deck = drawState.deck
      val visibleDeck = drawState.visibleDeck
      val nbCardsDistributed = drawState.nbCardsDistributed
      val playerIterator = drawState.playerIterator

      val playerToGiveToOption: Option[(ActorRef, PlayerSession)] = playerIterator.nextOption()
      playerToGiveToOption match {
        /**
         * update State with:
         *  - card removed from the deck,
         *  - the same Iterator to keep trace of the current Player to give the card to,
         *  - the number of distributed card incremented
         **/
        case Some((playerRef, _)) =>
          val cardToGive = drawState.deck.cards.take(1)
          val i = nbCardsDistributed+1
          val nextDrawState = GameSupervisorActor.DrawState(phase, deck.cards.drop(1), visibleDeck, i, playerIterator)
          playerRef ! Messages.Game.GiveCard(cardToGive.head, phase, i)
          context become distributing(nextDrawState)
          self ! Messages.Game.DrawTime

        /**
         * if iterator return None, then we completely looped over the players list :
         *  - iterate again to give one more card to all players, aka with a new players Iterator
         *  - end of distributing phase
         **/
        case None =>
          drawState.nbCardsDistributed match {
            case n if n < players.size * drawState.phase.nbCard =>
              val nextDrawState = GameSupervisorActor.DrawState(phase, deck, visibleDeck, nbCardsDistributed, players.iterator)
              context become distributing(nextDrawState)
              self ! Messages.Game.DrawTime

            /**
             * When ending a distributing phase, we need to create a GameState with the correct values:
             *  - initialize the first player info with an empty hand
             *  - change the state of the GameSupervisorActor to 'playing'
             */
            case n if n == players.size * drawState.phase.nbCard =>
              val initialGameState = GameSupervisorActor.GameState(FirstDraw, deck, Map.empty, players.iterator)
              context become playing(initialGameState)
              self ! Messages.Game.PlayTime

            case _ => throw new RuntimeException("Error: should not distribute more cards to the players")
          }
      }

    case Messages.Game.PlayTime =>
      sender ! UnavailableRequest
  }

  def playing(gameState: GameSupervisorActor.GameState): Receive = {
    case t: Messages.Game.Terminate =>
      println(s"Terminating the game ${room.name} due to ${t.reason}")
      val currentPlayerOption = gameState.playerIterator.nextOption()
      currentPlayerOption match {
        case Some((currentPlayerRef, currentPlayer)) =>
          currentPlayerRef ! t
          players
            .filterNot(currentPlayer == _._2)
            .foreachEntry((others, _) => others ! t)
          self ! PoisonPill
        case None => self ! PoisonPill
      }

    case Terminated(ref: ActorRef) if players.isDefinedAt(ref) => onPlayerLeft(ref)
    case Messages.Game.Leave if players.isDefinedAt(sender) => onPlayerLeft(sender)

    case Messages.Game.PlayTime =>
      val phase = gameState.phase
      val deck = gameState.deck
      val visibleDeck = gameState.visibleDeck
      val playerIterator = gameState.playerIterator
      val currentPlayerOption = playerIterator.nextOption()

      /**
       * If the playerIterator contains a next Player we need to:
       *  - ask the current Player his moves, aka his Cards and the dropped Card
       *  - update the gameState accordingly
       **/
      currentPlayerOption match {
        case Some((currentPlayerRef, currentPlayerSession)) =>
          val playerVisibleDeck = visibleDeck(currentPlayerSession)

          val playerMovesFuture = currentPlayerRef ? Messages.Game.AskMoves(phase) // TODO

          playerMovesFuture onComplete {
            case Success((playerNewCards: PlayerDeck, _)) => // TODO
              val playerNewDeck = PlayerDeck(playerVisibleDeck.deck ++ playerNewCards.deck)
              val allVisibleDecks = gameState.visibleDeck ++ Map(currentPlayerSession -> playerNewDeck)
              val updatedGameState = GameSupervisorActor.GameState(phase, deck, allVisibleDecks, playerIterator)
              context become playing(updatedGameState)
              self ! Messages.Game.PlayTime

            // TODO Retry mechanism
            case Failure(e) =>
              println(s"$currentPlayerSession: invalid moves... ${e.getMessage}\nPlease retry")

          }

        /**
         * If the playerIterator does not contain a next Player, the Playing phase is over. We need to determine the next phase:
         *  - if the current phase is FifthDraw, we need to end the Game and compute scores
         *  - if not, we need to move on to the next Distribute phase and set up a new DistributeState with a reset player Iterator
         */
        case None =>
          phase match {
            case FifthDraw =>
              context become computeScore(gameState.visibleDeck)
              self ! Messages.Score.GetBoardPointWinner
            case _ =>
              val nextInitialDistributeState = GameSupervisorActor.DrawState(phase.next, deck, visibleDeck, 0, players.iterator)
              context become distributing(nextInitialDistributeState)
              self ! Messages.Game.DrawTime
          }
      }
  }

  def isSorted[T](s: Seq[T])(implicit ord: Ordering[T]): Boolean = s match {
    case Seq() => true
    case Seq(_) => true
    case _ => s.sliding(2).forall { case Seq(x, y) => ord.lteq(x, y) }
  }

  def computeScore(visibleDeck: Map[PlayerSession, PlayerDeck]): Receive = {
    case Messages.Score.GetBoardPointWinner =>
      val scoreComputer = context.actorOf(ScoreEngineActor.props(visibleDeck), "score-engine")

      val handsScoresPerPositionPerPlayer = for {
        (playerSession, playerDeck) <- visibleDeck
      } yield (playerSession, playerDeck.deck.map(deck => (deck._1, Hand(deck._2))))

      val playersScoresPerRow = handsScoresPerPositionPerPlayer.map {
        case (playerSession, playerPositionDeck) => playerPositionDeck map {
          case (position, hand) =>
            val evaluatedHand = Hand.evaluate(hand, position)
            (playerSession, (evaluatedHand.evaluateScore(hand), evaluatedHand.royalties))
        }
      }

      val validPlayers = playersScoresPerRow.filter(playerScores => playerScores.values.toSeq match {
        case Seq() => true
        case Seq(_) => true
        case s => s.sliding(2).forall {
          case Seq(playerScoreRoyalties1, playerScoreRoyalties2) =>
            playerScoreRoyalties1._1 < playerScoreRoyalties2._1
        }
      })

      val invalidPlayers = players.toList.diff(validPlayers.toList)

      val scoresPair = for {
        (player1, index1) <- validPlayers.zipWithIndex
        (player2, index2) <- validPlayers.zipWithIndex
        if index1 < index2
      } yield (player1.toSeq, player2.toSeq)

      val winnersBoardPoint = scoreComputer ? Messages.Score.GetBoardPointWinner(scoresPair.toSeq)

    /*
        val handsRoyaltiesPerPositionPerPlayer = for {
          (playerSession, playerDeck) <- visibleDeck
        } yield (playerSession, playerDeck.deck.map { positionDeck =>
          (positionDeck._1, Hand.evaluate(Hand(positionDeck._2), positionDeck._1).royalties)
        })

        val totalRoyaltiesPerPlayer = handsRoyaltiesPerPositionPerPlayer map {
          case (playerSession, royaltiesPerPosition) =>
            if(invalidPlayers.contains((playerSession.ref, playerSession)))
              (playerSession, 0)
            else
              (playerSession, royaltiesPerPosition.values.sum)
        }

        val royaltiesPair = for {
          (player1, index1) <- totalRoyaltiesPerPlayer.zipWithIndex
          (player2, index2) <- totalRoyaltiesPerPlayer.zipWithIndex
          if index1 < index2
        } yield (player1, player2)

        val royalties = scoreComputer ? Messages.Score.ComputeRoyalties(royaltiesPair.toSeq)

        playersScoresPerRow map (scores => scores.forall())

        val playerPair = for {
          (p1, (h1Position, p1Hand)) <- handsScoresPerPositionPerPlayer
          (p2, (h2Position, p2Hand)) <- handsScoresPerPositionPerPlayer
          score <-
        } yield (p1, p2)

        val score = scoreComputer ? Messages.Game.ComputeScore(handsScoresPerPositionPerPlayer)
        ???*/


  }


  def onPlayerLeft(ref: ActorRef) = {
    val session = players(ref)
    println(s"Player ${session.player} has left the game")
    self ! Messages.Game.Terminate(session.player)
  }

  override def receive: Receive = initializing




  /*
    def finished(pending: Set[ActorRef]): Receive = {
      case Messages.Player.Accept if players.isDefinedAt(sender) => {
        val newPending = pending - sender
        val player = players(sender)
        println(s"Player ${player.player} has agreed to rematch")

        if(newPending isEmpty) {
          println("Restarting game...")
          val sessions = players.values

          sessions.foreach(player => player.ref ! Messages.Game.Restart)

          startGame(sessions.toList)
        }
      }
    }
  */

}
