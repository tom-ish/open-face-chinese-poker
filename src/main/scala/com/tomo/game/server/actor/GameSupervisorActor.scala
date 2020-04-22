package com.tomo.game.server.actor

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import com.tomo.game.Messages
import com.tomo.game.domain._
import com.tomo.game.server.PlayerSession
import com.tomo.game.server.actor.GameSupervisorActor.UnavailableRequest

import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object GameSupervisorActor {

  case class GameState(phase: Phase, deck: CardStack, visibleDeck: Map[PlayerSession, PlayerDeck], playerIterator: Iterator[(ActorRef, PlayerSession)])
  case class DrawState(phase: Phase, deck: CardStack, visibleDeck: Map[PlayerSession, PlayerDeck], nbCardsDistributed: Int, playerIterator: Iterator[(ActorRef, PlayerSession)])
  case class StateResult(newState: GameRoomActor.GameState)
  case object UnavailableRequest

  object Messages {
    case class ReceivePlayers(players: List[PlayerSession])
  }

  def props(gameRoom: GameRoom) = Props(new GameSupervisorActor(gameRoom))
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
        players = players + (p.ref -> p)
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
            case players.size * drawState.phase.nbCard =>
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
            case Success((playerNewCards: PlayerDeck, dropCard: Option[Card])) => // TODO
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
              self ! Messages.Game.ComputeScore
            case _ =>
              val nextInitialDistributeState = GameSupervisorActor.DrawState(phase.next, deck, visibleDeck, 0, players.iterator)
              context become distributing(nextInitialDistributeState)
              self ! Messages.Game.DrawTime
          }
      }
  }

  def computeScore(visibleDeck: Map[PlayerSession, PlayerDeck]): Receive = {
    val scoreComputer = context.actorOf(ScoreEngineActor.props(visibleDeck), "score-engine")
    val score = scoreComputer ? Messages.Game.ComputeScore
  }


  def onPlayerLeft(ref: ActorRef) = {
    val session = players(ref)
    println(s"Player ${session.player} has left the game")
    self ! Messages.Game.Terminate(session.player)
  }

  override def receive: Receive = initializing
}
