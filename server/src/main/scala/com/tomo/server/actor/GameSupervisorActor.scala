package main.scala.com.tomo.game.server.actor

import akka.actor
import akka.actor.{Actor, ActorRef, DiagnosticActorLogging, PoisonPill, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import com.tomo.server.actor.ScoreEngineActor
import main.scala.com.tomo.common.Messages
import main.scala.com.tomo.common.domain._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps

object GameSupervisorActor {

  case class GameState(phase: Phase, deck: CardStack, visibleDeck: Map[PlayerSession, PlayerDeck], playersHands: Map[PlayerSession, CardStack], currentPlayer: Option[(ActorRef, PlayerSession)], playerIterator: Iterator[(ActorRef, PlayerSession)])
  case class DrawState(phase: Phase, deck: CardStack, visibleDeck: Map[PlayerSession, PlayerDeck], playersHands: Map[PlayerSession, CardStack], nbCardsDistributed: Int, playerIterator: Iterator[(ActorRef, PlayerSession)])
  case object UnavailableRequest

  object Messages {
    case class ReceivePlayers(players: List[PlayerSession])
  }

  def props(gameRoom: GameRoom) = actor.Props(new GameSupervisorActor(gameRoom))
}

class GameSupervisorActor(val room: GameRoom) extends Actor with DiagnosticActorLogging {
  implicit val timeout: Timeout = 5 seconds
  implicit val executionContext = context.dispatcher

  var players: mutable.LinkedHashMap[ActorRef, PlayerSession] = mutable.LinkedHashMap()
  var playersHands: Map[PlayerSession, CardStack] = Map.empty

  def startGame(playersSessions: List[PlayerSession]) = {
    /* broadcast to all players that the other players joined the room */
    playersSessions.foreach(_.ref ! Messages.Game.Joined(room))

    val deck = CardStack.shuffled.cards

    // INTRODUCE PLAYERS
    playersSessions.foreach(_.ref ! Messages.Game.SetUp(playersSessions.map(_.player)))

    val emptyVisibleDeck = playersSessions map(_ -> PlayerDeck.empty)
    val emptyPlayersHands = playersSessions map (_ -> CardStack.empty)
    val initialDrawState = GameSupervisorActor.DrawState(FirstDraw, deck, emptyVisibleDeck.toMap, emptyPlayersHands.toMap, 0, players.iterator)
    context become distributing(initialDrawState)
    self ! Messages.Game.DrawTime
  }

  def initializing: Receive = {
    case GameSupervisorActor.Messages.ReceivePlayers(playersList: List[PlayerSession]) => {
      playersList.foreach { p =>
        log.info(s"received $p")
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
      val playersHands = drawState.playersHands
      val nbCardsDistributed = drawState.nbCardsDistributed
      val playerIterator = drawState.playerIterator

      val playerToGiveToOption = playerIterator.nextOption()
      playerToGiveToOption match {
        /**
         * update State with:
         *  - card removed from the deck,
         *  - the same Iterator to keep trace of the current Player to give the card to,
         *  - the number of distributed card incremented
         **/
        case Some((playerRef, playerSession)) =>
          val cardToGive = drawState.deck.cards.take(1)
          val playerHand = CardStack(playersHands(playerSession).cards ++ cardToGive)
          val newPlayersHands = playersHands + (playerSession -> playerHand)
          val nextDrawState = GameSupervisorActor.DrawState(phase, deck.cards.drop(1), visibleDeck, newPlayersHands, nbCardsDistributed, playerIterator)
          playerRef ! Messages.Game.GiveCard(cardToGive.head, phase, nbCardsDistributed)
          log.info(s"[$phase] gave card ${cardToGive.head} to ${playerSession.player.name}")
          context become distributing(nextDrawState)
          self ! Messages.Game.DrawTime

        /**
         * if iterator return None, then we completely looped over the players list :
         *  - iterate again to give one more card to all players
         *  - end of distributing phase
         **/
        case None =>
          val nbCardsDistributed = drawState.nbCardsDistributed + 1
          log.info("players list has been looped entirely...")
          log.info(s"distributed $nbCardsDistributed cards")
          log.info(playersHands.toString())
          nbCardsDistributed match {
            case i if i < drawState.phase.nbCard =>
              log.info("distributing one more card to all")
              val nextDrawState = GameSupervisorActor.DrawState(phase, deck, visibleDeck, playersHands, nbCardsDistributed, players.iterator)
              context become distributing(nextDrawState)
              self ! Messages.Game.DrawTime

            /**
             * When ending a distributing phase, we need to create a GameState with the correct values:
             *  - initialize the first player info with an empty hand
             *  - change the state of the GameSupervisorActor to 'playing'
             */
            case i if i == drawState.phase.nbCard =>
              log.info("PLAY TIME!")
              val initialGameState = GameSupervisorActor.GameState(phase, deck, visibleDeck, playersHands, None, players.iterator)
              context become playing(initialGameState)
              players.foreach(_._1 ! Messages.Game.PlayTime)
              self ! Messages.Game.PlayTime

            case _ => throw new RuntimeException("Error: should not distribute more cards to the players")
          }
      }
  }

  def playing(gameState: GameSupervisorActor.GameState): Receive = {
    case t: Messages.Game.Terminate =>
      log.info(s"Terminating the game ${room.name} due to ${t.reason}")
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


      /**
       * If the playerIterator contains a next Player we need to:
       *  - ask the current Player his moves, aka his Cards and the dropped Card
       * If not, the Playing phase is over. We need to determine the next phase:
       *  - if the current phase is FifthDraw, we need to end the Game and compute scores
       *  - if not, we need to move on to the next Distribute phase and set up a new DistributeState with a reset player Iterator
       */
    case Messages.Game.PlayTime =>
      val phase = gameState.phase
      val visibleDeck = gameState.visibleDeck
      val playersHands = gameState.playersHands
      val currentPlayerOption = gameState.playerIterator.nextOption()
      currentPlayerOption match {
        case Some(currentPlayer) =>
          visibleDeck.foreach(m => log.info(m.toString))
          log.info(s"Asking moves to ${currentPlayer._2.player.name}...")
          currentPlayer._1 ! Messages.Game.PlayerTurn(phase)
          players
            .filterNot(_ == currentPlayer)
            .foreach(_._1 ! Messages.Game.NotYourTurn(currentPlayer._2.player))
          val newGameState = GameSupervisorActor.GameState(gameState.phase, gameState.deck, gameState.visibleDeck, playersHands, currentPlayerOption, gameState.playerIterator)
          context become playing(newGameState)
          currentPlayer._1 ! Messages.Game.AskMoves

        case None =>
          phase match {
            case FifthDraw =>
              context become computeScore(gameState.visibleDeck)
              players.foreach(_._1 ! Messages.Game.ScoreTime)
              self ! Messages.Score.GetBoardPointWinner
            case _ =>
              val newEmptyPlayersHands = players.map(_._2 -> CardStack.empty).toMap
              val nextInitialDistributeState = GameSupervisorActor.DrawState(phase.next, gameState.deck, visibleDeck, newEmptyPlayersHands, 0, players.iterator)
              context become distributing(nextInitialDistributeState)
              players.foreach(_._1 ! Messages.Game.DrawTime)
              self ! Messages.Game.DrawTime
          }
      }

    case Messages.Player.PlayerMoves(positionMoves) =>
      val visibleDeck = gameState.visibleDeck
      val playersHands = gameState.playersHands
      val currentPlayerOption = gameState.currentPlayer
      val positionToPlay = positionMoves._1
      val cardsToPlay = positionMoves._2

      currentPlayerOption match {
        case None =>
          log.error("Should not go there...")
          throw new RuntimeException("Error: currentPlayer is empty...")

        case Some(player) if player._1 == sender =>
          val playerSession = player._2
          val playerVisibleDeck = visibleDeck(playerSession)
          val cardsInHand = playersHands(playerSession)

          if (cardsToPlay.forall(cardsInHand.cards.contains(_))) {
            log.info(s"${playerSession.player.name} has just played: $positionMoves")
            val newPlayerHand = CardStack(playersHands(playerSession).cards diff positionMoves._2)
            val newPlayersHands = playersHands + (playerSession -> newPlayerHand)

            // the corresponding position cards visible by all
            val playerPositionCards = playerVisibleDeck.deck(positionToPlay)
            // the corresponding position cards + the cards played for the same position
            val newPlayerPositionCards = playerPositionCards.cards ++ cardsToPlay
            log.info(s"Previous Cards: $positionToPlay -> $playerPositionCards")
            log.info(s"New Cards: $positionToPlay -> $newPlayerPositionCards")
            // the global player deck visible by all
            val playerNewDeck = PlayerDeck(playerVisibleDeck.deck ++ Map(positionToPlay -> CardStack(newPlayerPositionCards)))
            // the global decks of all players
            val allVisibleDecks = gameState.visibleDeck ++ Map(player._2 -> playerNewDeck)

            val updatedGameState = GameSupervisorActor.GameState(gameState.phase, gameState.deck, allVisibleDecks, newPlayersHands, currentPlayerOption, gameState.playerIterator)
            context become playing(updatedGameState)

            if(newPlayerHand.isEmpty) { // => End of current player's turn
              log.info(s"${player._2.player} finished his turn.")
              player._1 ! Messages.Game.PlayerTurnEnded
              players
                .filterNot(_._1 == player._1)
                .foreach(_._1 ! Messages.Game.UpdateGameState(allVisibleDecks.map(deck => deck._1.player -> deck._2), player._2.player))
              self ! Messages.Game.PlayTime
            } else {  // Ask More Moves
              log.info(s"Still ${playerSession.player.name}'s turn...'")
              self ! Messages.Game.AskMovesAgain(player._1)
            }
          } else {
            log.info(s"$playerSession is trying to play some cards that he does not own")
            log.error("should not go there...")
          }

        case Some(e) =>
          log.info(s"why ? > ${e._2.player.name}")
          sender ! Messages.Game.NotYourTurn
      }
    case Messages.Player.PlayerInvalidInput =>
      log.info(s"$sender: invalid moves... Please retry")
      self ! Messages.Game.AskMovesAgain(sender)

    case Messages.Game.AskMovesAgain(player) =>
      val phase = gameState.phase
      val currentPlayerOption = gameState.currentPlayer
      currentPlayerOption match {
        case Some(currentPlayer) if currentPlayer._1 == player =>
          log.info(s"Asking more moves to ${currentPlayer._2.player.name}")
          val newGameState = GameSupervisorActor.GameState(gameState.phase, gameState.deck, gameState.visibleDeck, gameState.playersHands, currentPlayerOption, gameState.playerIterator)
          context become playing(newGameState)
          currentPlayer._1 ! Messages.Game.AskMoves
        case Some(_) =>
          sender ! Messages.Game.NotYourTurn
        case None =>
          log.error("Error: should't go there after user invalid inputs")
      }

      // TODO
/*    case Failure(e) => // User Input was not sent in time
      log.info("Failure on getting player moves...")
      log.info("")
      self ! Messages.Game.PlayTime*/
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
    log.info(s"Player ${session.player} has left the game")
    self ! Messages.Game.Terminate(session.player)
  }

  override def receive: Receive = initializing




  /*
    def finished(pending: Set[ActorRef]): Receive = {
      case Messages.Player.Accept if players.isDefinedAt(sender) => {
        val newPending = pending - sender
        val player = players(sender)
        log.info(s"Player ${player.player} has agreed to rematch")

        if(newPending isEmpty) {
          log.info("Restarting game...")
          val sessions = players.values

          sessions.foreach(player => player.ref ! Messages.Game.Restart)

          startGame(sessions.toList)
        }
      }
    }
  */

}
