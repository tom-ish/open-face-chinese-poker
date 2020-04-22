package com.tomo.game.server.actor

import akka.actor.{Actor, ActorRef, PoisonPill, Props, Terminated}
import com.tomo.game.Messages
import com.tomo.game.domain.Rank.Five
import com.tomo.game.domain.{Card, CardStack, FifthDraw, FirstDraw, FourthDraw, GameRoom, NotPlaying, Phase, PlayerState, ScoringPhase, SecondDraw, ThirdDraw}
import com.tomo.game.server.PlayerSession
import com.tomo.game.server.actor.GameRoomActor.{GameState, PlayerInformation, StateResult}

import language.postfixOps
import scala.util.{Failure, Success, Try}

object GameRoomActor {
  case class PlayerInformation(session: PlayerSession, state: PlayerState)
  case class GameState(phase: Phase, deck: CardStack, playerInTurnInfo: PlayerInformation, opponentsInfo: List[PlayerInformation])
  case class StateResult(newState: GameRoomActor.GameState)

  object Messages {
    case class ReceivePlayers(players: List[PlayerSession])
  }

  def determineNextState(phase: Phase, playerCardStack: PlayerCardStack, playerInfo: PlayerInformation, nextPlayerInfo: PlayerInformation, value: List[Card]): StateResult = {
    val player = playerInfo.state
    val nextPlayer = nextPlayerInfo.state

    val isFinished = phase == FifthDraw
    val nextPhase = phase match {
      case FirstDraw => SecondDraw
      case SecondDraw => ThirdDraw
      case ThirdDraw => FourthDraw
      case FourthDraw => FifthDraw
      case FifthDraw => ScoringPhase
      case ScoringPhase => NotPlaying
    }

    val nextState = GameRoomActor.GameState(nextPhase, )

    StateResult(nextState)
  }

  def props(room: GameRoom): Props = Props(new GameRoomActor(room))
}

class GameRoomActor(val room: GameRoom) extends Actor {
  var players: Map[ActorRef, PlayerSession] = Map()

  def startGame(playerSessions: List[PlayerSession]) = {
    playerSessions.foreach(_.ref ! Messages.Game.Joined(room))

    val deck = CardStack.shuffled.cards
    val cardsToGive = deck.take(FirstDraw.nbCard * playerSessions.size)
    cardsToGive map { card: Card =>
      playerSessions map (_.ref ! Messages.Game.GiveCard(card))
    }
    val remainder = deck.drop(FirstDraw.nbCard * playerSessions.size)
    val initialState = GameRoomActor.GameState(FirstDraw, remainder, playerSessions.head, playerSessions.filterNot(_ == playerSessions.head))
    context become playing(initialState)

    // ANNOUNCE CURRENT PLAYER
    val playersInformation = playerSessions.map { playerSession: PlayerSession =>
      GameRoomActor.PlayerInformation(playerSession, PlayerState())
    }

    playersInformation.foreach { playerInfo: PlayerInformation =>
      playerInfo.session.ref ! Messages.Game.SetUp
    }

  }

  def getNextState(player: ActorRef, playerCardStack: PlayerCardStack, currentState: GameRoomActor.GameState): Try[GameRoomActor.StateResult] = {
    if(currentState.playerInTurnInfo.session.ref != player) {
      Failure(new Exception("Please wait until it is your turn."))
    } // else if (!currentState.playerInTurnInfo.state.playerCardStack.hand.cards.contains())
      // TODO Check that the player really owns all the cards he added to its deck
    else if (!currentState.playerInTurnInfo.state.playerCardStack.hand.cards.contains(playerCardStack)) {
      Failure(new Exception("You have played an invalid card."))
    } else {
      val player = currentState.playerInTurnInfo
      val nextPlayer = currentState.opponentsInfo.head
      val deckCards = currentState.deck.cards
      val phase = currentState.phase

      val result = GameRoomActor.determineNextState(phase, playerCardStack, player, nextPlayer, deckCards)
      Success(result)
    }
  }

  /*
    A player has left. Terminate the game
   */
  def onPlayerLeft(ref: ActorRef): Unit = {
    val session = players(ref)
    println(s"Player ${session.player} has left the game")
    self ! Messages.Game.Terminate(session.player)
  }

  /*
    Initial state: wait for the matchmaking server to introduce the players
   */
  def initializing: Receive = {
    case GameRoomActor.Messages.ReceivePlayers(waitingPlayers: List[PlayerSession]) => {
      waitingPlayers.foreach { p =>
        players = players + (p.ref -> p)
        context.watch(p.ref)
      }
      startGame(waitingPlayers)
    }
  }

  /*
    The Game is being played
   */
  def playing(state: GameRoomActor.GameState): Receive = {
    case t: Messages.Game.Terminate => {
      println(s"Terminating the game ${room.name} due to ${t.reason}")

      state.playerInTurnInfo.session.ref ! t
      state.opponentsInfo.foreach(_.session.ref ! t)

      self ! PoisonPill
    }

    case Terminated(ref: ActorRef) if players.isDefinedAt(ref) => onPlayerLeft(ref)
    case Messages.Game.Leave if players.isDefinedAt(sender) => onPlayerLeft(sender)

/*
    case Messages.Game.DrawTime(phase) =>
      println(s"Draw time > $phase")
*/

    case Messages.Game.CardsPlayed(playerCardsStack) if players.isDefinedAt(sender) =>
      println(s"Player $sender played $playerCardsStack")
      getNextState(sender, playerCardsStack, state) match {
          //case class GameState(deck: CardStack, playerInTurn: PlayerInformation, playersWaiting: List[PlayerInformation])
        case Success(GameRoomActor.StateResult(state: GameState)) =>
          // Player made a move, aka played X cards
          val nextPlayer = state.playerInTurnInfo
          val previousPlayer = state.opponentsInfo

          // TODO: Broadcast all players their decks
          println(s"Player Move accepted: moving to new turn. Player in turn : ${nextPlayer.state}")

          context become playing(state)
        case Failure(exception) => sender ! Messages.Server.Message(exception.getMessage)
      }
  }

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

  override def receive: Receive = initializing
}
