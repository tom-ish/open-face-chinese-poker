package com.tomo.server.actor

import akka.actor
import akka.actor.{Actor, ActorRef, DiagnosticActorLogging, PoisonPill, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import com.tomo.server.actor.GameSupervisorOptActor.PlayerState
import main.scala.com.tomo.common.Messages
import main.scala.com.tomo.common.domain._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps

object GameSupervisorOptActor {

  case class GameState(phase: Phase, deck: CardStack, visibleBoard: Map[PlayerSession, PlayerBoard], playersStates: Map[PlayerSession, PlayerState], currentPlayer: Option[(ActorRef, PlayerSession)], playerIterator: Iterator[(ActorRef, PlayerSession)])
  case class DrawState(phase: Phase, deck: CardStack, visibleBoards: Map[PlayerSession, PlayerBoard], playersStates: Map[PlayerSession, PlayerState], nbCardsDistributed: Int, playerIterator: Iterator[(ActorRef, PlayerSession)])
  case class PlayerState(hand: CardStack, droppedCards: List[Card], board: PlayerBoard, session: PlayerSession)

  object Messages {
    case class ReceivePlayers(players: List[PlayerSession])
  }

  def props(gameRoom: GameRoom) = actor.Props(new GameSupervisorOptActor(gameRoom))
}

class GameSupervisorOptActor(val room: GameRoom) extends Actor with DiagnosticActorLogging {
  implicit val timeout: Timeout = 5 seconds
  implicit val executionContext = context.dispatcher

  var players: mutable.LinkedHashMap[ActorRef, PlayerSession] = mutable.LinkedHashMap()

  def startGame(playersSessions: List[PlayerSession]) = {
    /* broadcast to all players that the other players joined the room */
    playersSessions.foreach(_.ref ! Messages.Game.Joined(room))

    val deck = CardStack.shuffled.cards

    // INTRODUCE PLAYERS
    playersSessions.foreach(_.ref ! Messages.Game.SetUp(playersSessions.map(_.player)))

    val emptyVisibleDeck = playersSessions map(_ -> PlayerBoard.empty)
    val initialPlayersStates = playersSessions map { playerSession =>
      playerSession -> GameSupervisorOptActor.PlayerState(CardStack.empty, List.empty, PlayerBoard.empty, playerSession)
    }
    val initialDrawState = GameSupervisorOptActor.DrawState(FirstDraw, deck, emptyVisibleDeck.toMap, initialPlayersStates.toMap, 0, players.iterator)
    context become distributing(initialDrawState)
    self ! Messages.Game.DrawTime
  }

  def initializing: Receive = {
    case GameSupervisorOptActor.Messages.ReceivePlayers(playersList: List[PlayerSession]) => {
      playersList.foreach { p =>
        log.info(s"received $p")
        players += (p.ref -> p)
        context.watch(p.ref)
      }
      startGame(playersList)
    }
  }

  def distributing(drawState: GameSupervisorOptActor.DrawState): Receive = {
    case Messages.Game.DrawTime =>
      val phase = drawState.phase
      val deck = drawState.deck
      val visibleBoards = drawState.visibleBoards
      val playersStates = drawState.playersStates
      val nbCardsDistributed = drawState.nbCardsDistributed
      val playerIterator = drawState.playerIterator

      val playerTurnOption = playerIterator.nextOption()
      playerTurnOption match {
        /**
         * update State with:
         *  - card removed from the deck,
         *  - the same Iterator to keep trace of the current Player to give the card to,
         *  - the number of distributed card incremented
         **/
        case Some((_, playerSession)) =>
          val cardToGive = drawState.deck.cards.take(1)
          val playerState = playersStates(playerSession)
          val playerHand = playerState.hand
          val newPlayerHand = CardStack(playerHand.cards ++ cardToGive)
          val newPlayerState = GameSupervisorOptActor.PlayerState(newPlayerHand, playerState.droppedCards, playerState.board, playerSession)
          val newPlayersStates = playersStates + (playerSession -> newPlayerState)
          val nextDrawState = GameSupervisorOptActor.DrawState(phase, deck.cards.drop(1), visibleBoards, newPlayersStates, nbCardsDistributed, playerIterator)
          log.info(s"[$phase] ${cardToGive.head} to ${playerSession.player.name}")
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
          nbCardsDistributed match {
            case i if i < drawState.phase.nbCard =>
              log.info("distributing one more card to all")
              val nextDrawState = GameSupervisorOptActor.DrawState(phase, deck, visibleBoards, playersStates, nbCardsDistributed, players.iterator)
              context become distributing(nextDrawState)
              self ! Messages.Game.DrawTime

            /**
             * When ending a distributing phase, we need to create a GameState with the correct values:
             *  - initialize the first player info with an empty hand
             *  - change the state of the GameSupervisorOptActor to 'playing'
             */
            case i if i == drawState.phase.nbCard =>
              playersStates foreachEntry { (playerSession, playerState) =>
                log.info(s"Sent cards to ${playerSession.player.name}: ${playerState.hand}")
                playerSession.ref ! Messages.Game.SendHand(playerState.hand, phase)
              }

              log.info("PLAY TIME!")
              val initialGameState = GameSupervisorOptActor.GameState(phase, deck, visibleBoards, playersStates, None, players.iterator)
              context become playing(initialGameState)
              self ! Messages.Game.PlayTime

            case _ => throw new RuntimeException("Error: should not distribute more cards to the players")
          }
      }
  }

  def playing(gameState: GameSupervisorOptActor.GameState): Receive = {
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
      val visibleBoard = gameState.visibleBoard
      val playersStates = gameState.playersStates
      val currentPlayerOption = gameState.playerIterator.nextOption()
      currentPlayerOption match {
        case Some(currentPlayer) =>
          visibleBoard.foreach(board => log.info(board.toString))
          players
            .filterNot(_ == currentPlayer)
            .foreach(_._1 ! Messages.Game.NotYourTurn(currentPlayer._2.player))
          log.info(s"Asking moves to ${currentPlayer._2.player.name}...")
          currentPlayer._1 ! Messages.Game.PlayerTurn(phase)
          val newGameState = GameSupervisorOptActor.GameState(gameState.phase, gameState.deck, gameState.visibleBoard, playersStates, currentPlayerOption, gameState.playerIterator)
          context become playing(newGameState)
          currentPlayer._1 ! Messages.Game.AskMoves(phase)

        case None =>
          phase match {
            case FifthDraw =>
              context become computeScore(gameState.visibleBoard)
              players.foreach(_._1 ! Messages.Game.ScoreTime)
              self ! Messages.Score.GetBoardPointWinner
            case _ =>
              val newPlayersStates = playersStates map {
                case (session, state) => session -> PlayerState(CardStack.empty, state.droppedCards, state.board, session)
              }
              val nextInitialDrawState = GameSupervisorOptActor.DrawState(phase.next, gameState.deck, visibleBoard, newPlayersStates, 0, players.iterator)
              context become distributing(nextInitialDrawState)
              players.foreach(_._1 ! Messages.Game.DrawTime)
              self ! Messages.Game.DrawTime
          }
      }

    case Messages.Player.PlayerCompleteMoves(positionMoves) =>
      val phase = gameState.phase
      val visibleBoard = gameState.visibleBoard
      val playersStates = gameState.playersStates
      val currentPlayerOption = gameState.currentPlayer

      currentPlayerOption match {
        case None =>
          log.error("Should not go there...")
          throw new RuntimeException("Error: currentPlayer is empty...")

        case Some(player) if player._1 == sender =>
          val playerSession = player._2
          val playerState = playersStates(playerSession)
          val playerBoard = playerState.board
          val cardsInHand = playerState.hand
          val cardsDropped = playerState.droppedCards

          log.info(s"${playerSession.player.name} has just played: $positionMoves")

          if (isValid(phase, positionMoves, playerState)) {
            val newPlayerBoard = positionMoves map {
              case (position, moves) =>
                position -> CardStack(playerBoard.positionCardStack(position).cards ++ moves)
            }
            val newDroppedCards = cardsDropped ++ positionMoves(DroppedCard)
            val newPlayerState = GameSupervisorOptActor.PlayerState(CardStack.empty, newDroppedCards, newPlayerBoard, playerSession)
            val newPlayersStates = playersStates + (playerSession -> newPlayerState)
            val newVisibleBoards = visibleBoard + (playerSession -> PlayerBoard(newPlayerBoard))
            val newGameState = GameSupervisorOptActor.GameState(phase, gameState.deck, newVisibleBoards, newPlayersStates, currentPlayerOption, gameState.playerIterator)
            context become playing(newGameState)

            log.info(s"${playerSession.player.name} play was accepted")
            // notify current player that his move was accepted
            player._1 ! Messages.Game.MovesAccepted
            // notify others to update the gameBoard
            players
              .filterNot(_ == player)
              .foreach { pl =>
                val updatedBoard = newVisibleBoards.map (playerBoard => playerBoard._1.player -> playerBoard._2)
                log.info(s"NEW BOARD: ")
                updatedBoard.foreach { p => log.info(s"================= ${p._1.name} =================")
                  p._2.positionCardStack.foreach(po => log.info(s"${po._1} = ${po._2}"))
                }
                pl._1 ! Messages.Game.UpdateGameState(updatedBoard, player._2.player)
              }
            self ! Messages.Game.PlayTime
          }
          else {
            val msg = phase match {
              case FirstDraw if positionMoves(DroppedCard).nonEmpty =>
                "no card can be dropped in First Draw"
                // notify player that his play is invalid
              case _ if positionMoves(DroppedCard).isEmpty =>
                "no card was dropped this turn"
              case _ if positionMoves.values.flatten.toSet.size != phase.nbCard =>
                "too much/not enough cards to finish the turn"
              case _ => positionMoves.map {
                case (position, cards) if cards.size + playerBoard.positionCardStack(position).cards.size > position.nbCards =>
                  s"too much cards on $position"
                case _ if !positionMoves.values.forall(cardsInHand.cards.contains(_)) =>
                  "trying to play cards not in hand"
                case _ =>
                  "unknown..."
              }
            }
            log.info(s"$msg")
            player._1 ! Messages.Game.InvalidInputs(phase, s"$msg", player._2)
            self ! Messages.Game.InvalidInputs(phase, s"$msg", player._2)
          }

        case Some(e) =>
          log.info(s"why ? > ${e._2.player.name}")
          sender ! Messages.Game.NotYourTurn
      }

    case Messages.Game.InvalidInputs(phase, reason, player) =>
      log.info(s"Invalid inputs from ${player.player.name}: $reason\n${gameState.playersStates(player)}")
      self ! Messages.Game.AskMovesAgain(player.ref)

    case Messages.Game.AskMovesAgain(player) =>
      val currentPlayerOption = gameState.currentPlayer
      currentPlayerOption match {
        case Some(currentPlayer) if currentPlayer._1 == player =>
          log.info(s"Asking moves again to ${currentPlayer._2.player.name}")
          val newGameState = GameSupervisorOptActor.GameState(gameState.phase, gameState.deck, gameState.visibleBoard, gameState.playersStates, currentPlayerOption, gameState.playerIterator)
          context become playing(newGameState)
          currentPlayer._1 ! Messages.Game.AskMoves(gameState.phase)
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

  def isValid(phase: Phase, positionMoves: Map[Position, List[Card]], playerState: PlayerState): Boolean =
    positionMoves.values.flatten.toSet.size == phase.nbCard && // if the sum of all the cards in the moves is different from phase nbCards distributed => false
      positionMoves.forall { case (position, value) => isValid(phase, position, value, playerState)}


  def isValid(phase: Phase, playedPosition: Position, playedCards: List[Card], playerState: PlayerState): Boolean = {
    val playerHand = playerState.hand
    val playerBoard = playerState.board

    val allCardsFromHand = playedCards.forall(playerHand.cards.contains(_))
    val respectPositionMaxCards = playedCards.size + playerBoard.positionCardStack(playedPosition).cards.size <= playedPosition.nbCards

    phase match {
      case FirstDraw =>
        (playedPosition == DroppedCard && playedCards.isEmpty) || (allCardsFromHand && respectPositionMaxCards)
      case _ =>
        if(playedPosition == DroppedCard) playedCards.size == 1 && allCardsFromHand
        else allCardsFromHand && respectPositionMaxCards
    }
  }

  def isSorted[T](s: Seq[T])(implicit ord: Ordering[T]): Boolean = s match {
    case Seq() => true
    case Seq(_) => true
    case _ => s.sliding(2).forall { case Seq(x, y) => ord.lteq(x, y) }
  }

  def computeScore(visibleDeck: Map[PlayerSession, PlayerBoard]): Receive = {
    case Messages.Score.GetBoardPointWinner =>
      val scoreComputer = context.actorOf(ScoreEngineActor.props(visibleDeck), "score-engine")

      val handsScoresPerPositionPerPlayer = for {
        (playerSession, playerDeck) <- visibleDeck
      } yield (playerSession, playerDeck.positionCardStack.map { cardStackPosition =>
      if(cardStackPosition._1 == Top) (Top, Hand(cardStackPosition._2))
      else (cardStackPosition._1, Hand(cardStackPosition._2))
      })

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

  private def getPlayerFromSender(sender: ActorRef): PlayerSession = players.filter(_._1 == sender).head._2


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
