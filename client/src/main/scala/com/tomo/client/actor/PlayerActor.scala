package com.tomo.client.actor

import akka.actor.{Actor, ActorRef, DiagnosticActorLogging, PoisonPill, Props}
import akka.pattern.pipe
import main.scala.com.tomo.common.domain._
import main.scala.com.tomo.common.{Messages, Utils}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Failure, Success}

case class ServerFound(ref: ActorRef)
case class CardsPlayed(cards: List[Card])

object PlayerActor {
  def props = Props(new PlayerActor)
}

class PlayerActor extends Actor with DiagnosticActorLogging {
  implicit val executionContext: ExecutionContext = context.dispatcher

  /* the server currently joined */
  var server: ActorRef = null

  /* the gameRoom currently joined */
  var gameRoom: GameRoom = null

  /* actor reference to the currently joined gameRoom */
  var gameRoomRef: ActorRef = null

  /* the player */
  var me: Player = null

  /* the opponents */
  var opponents: List[Player] = List.empty

  /* the player hand */
  var myHand: CardStack = CardStack.empty

  /* all players decks */
  var visibleDecks: Map[Player, PlayerDeck] = null

  /* reset the game state. This is usually called before restarting the game */
  def resetState = {
    gameRoom = null
    opponents = List.empty
    visibleDecks = Map.empty
  }

  /* leave the room and reset the game state */
  def leaveRoom= {
    gameRoomRef ! Messages.Game.Leave
    resetState
    gameRoom = null
    gameRoomRef = null
  }

  /* the player is waiting to connect to a game lobby */
  def connecting: Receive = {
    case Messages.Server.Connected =>
      log.info("Connected to the server, waiting for opponents...")
      context become waiting
  }

  def waiting: Receive = {
    case Messages.Game.Joined(room) =>
      gameRoom = room
      gameRoomRef = sender

      log.info(s"You have joined the game room: ${gameRoom.name}")
      context become waitingToStart
  }

  def waitingToStart: Receive = {
    case Messages.Game.SetUp(players) =>
      opponents = players
      val playersDecks = players map (_ -> PlayerDeck.empty)
      visibleDecks = Map(me -> PlayerDeck.empty) ++ playersDecks.toMap
      log.info(s"You are playing against ${opponents.filterNot(_ == me).mkString(", ")}")
      context become drawing
  }

  def drawing: Receive = {
    case Messages.Game.GiveCard(card, phase, i) =>
      log.info(s"$phase | $card | $i")
      myHand = CardStack(myHand.cards ++ List(card))
      if (phase.nbCard == i+1) {
        val userInstructions =
          s""" ============ $phase ============
            | $myHand
            | ===================================
            |             Play Time
            | Please send your moves as follow:
            |   - T=indexesTop
            |   - M=indexesMiddle
            |   - B=indexesBottom
            | Each indexes corresponds to the
            | index of the card in your hand:
            |""".stripMargin

        val cardsWithIndex = myHand.cards.zipWithIndex
        log.info(userInstructions)
        cardsWithIndex.foreach(cardIndex => log.info(s"${cardIndex._2} - ${cardIndex._1}"))
        visibleDecks(me).deck.foreachEntry((position, stack) => log.info(s"$position = [${stack.cards}]"))
        log.info("===================================")
      }
    case Messages.Game.PlayTime =>
      context become play
  }

  def play: Receive = {
    case Messages.Game.Terminate(reason) => reason match {
      case reason: Messages.Game.Terminate.Reason.PlayerLeft if reason.player != me =>
        log.info("Your opponents have left the game, you will be sent back to the lobby")
        leaveRoom
      case reason: Messages.Game.Terminate.Reason.ErrorOccured =>
        log.info(s"The game terminated due to an error, you will be sent back to the lobby. Reason: ${reason.error}")
        leaveRoom
    }
    case Messages.Game.DrawTime =>
      context become drawing
    case Messages.Game.ScoreTime =>
      context become scoring
    case Messages.Game.NotYourTurn(player) =>
      log.info(s"Please wait your turn, it's ${player.name}'turn.")
      context become notPlaying
    case Messages.Game.PlayerTurn(phase) =>
      log.info(s"[$phase] It's your turn.")
    case Messages.Game.PlayerTurnEnded =>
      log.info("Your turn is over. Please wait for the others to play")
      context become notPlaying
    case Messages.Game.AskMoves =>
      val cardsWithIndex = myHand.cards.zipWithIndex
      cardsWithIndex.foreach(cardIndex => log.info(s"${cardIndex._2} - ${cardIndex._1}"))
      val playerInputFuture = StdIn.readLine()
      Future(playerInputFuture)
        .map (Utils.readMove(_, myHand.cards) match {
          case Some(cardsPerPosition) =>
            log.info(s"$cardsPerPosition")
            val playedPosition = cardsPerPosition._1
            val playedCards = cardsPerPosition._2

            // Check if the moves does not break the rules of the max number of cards in hand
            if(isValidMove(cardsPerPosition, visibleDecks(me))) {
              // Update player's hand by removing the cards from inputs
              myHand = CardStack(myHand.cards diff cardsPerPosition._2)
              // Update player's deck by adding the played cards from inputs
              val newPositionVisibleDeck = visibleDecks(me).deck(playedPosition).cards ++ playedCards
              val newPlayerDeck = visibleDecks(me)
              val otherPlayersDecks = visibleDecks.filterNot(_._1 == me)
              visibleDecks = Map(me -> PlayerDeck(newPlayerDeck.deck ++ Map(playedPosition -> CardStack(newPositionVisibleDeck)))) ++ otherPlayersDecks
              visibleDecks.foreachEntry((player, deck) => log.info(s"$player : ${deck.deck}"))
              Messages.Player.PlayerMoves(cardsPerPosition)
            } else {
              Messages.Player.PlayerInvalidInput
            }
          case None =>
            log.info("Couldn't read your inputs")
            Messages.Player.PlayerInvalidInput
        })
        .recover{ e =>
          log.info(s"Error: ${e.getMessage}")
          Messages.Player.PlayerInvalidInput
        }
        .pipeTo(sender)
  }

  def notPlaying: Receive = {
    case Messages.Game.PlayerTurn(phase) =>
      log.info(s"[$phase] It's your turn.")
      context become play
    case Messages.Game.UpdateGameState(allVisibleDeck, lastPlayer) =>
      log.info(s"${lastPlayer.name} has just played. This is the updated board: $allVisibleDeck")
      visibleDecks = allVisibleDeck
    case Messages.Game.DrawTime =>
      context become drawing
    case Messages.Game.NotYourTurn(player) =>
      log.info(s"This is ${player.name} turn")
  }

  def playing: Receive = {
    case Messages.Game.Lost =>
      log.info("You lost the game...")
      context become finished
    case Messages.Game.Win =>
      log.info("You won the game !")
      context become finished
    case t: Messages.Game.Terminate => t.reason match {
      case reason: Messages.Game.Terminate.Reason.PlayerLeft if reason.player != me =>
        log.info("Your opponents have left the game, you will be sent back to the lobby")
        leaveRoom
      case reason: Messages.Game.Terminate.Reason.ErrorOccured =>
        log.info(s"The game terminated due to an error, you will be sent back to the lobby. Reason: ${reason.error}")
        leaveRoom
    }
    case CardsPlayed => log.info("Please wait until it is your turn")
    case Messages.Server.Message(msg) => log.info(s"[Server]: $msg")
  }

  def finished: Receive = playing orElse {
    case Messages.Game.Restart => {
      log.info("Opponent accepted rematch, starting a new round")
      resetState
      context become waitingToStart
    }
  }

  def scoring: Receive = {
    case _ =>
      log.info("Game ended => calculating score")
  }

  override def receive: Receive = connecting

  def isValidMove(cardsPerPosition: (Position, List[Card]), playerDeck: PlayerDeck): Boolean = {
    cardsPerPosition._1 match {
      case Top    if playerDeck.deck(Top).cards.size    + cardsPerPosition._2.size > 3 => false
      case Bottom if playerDeck.deck(Bottom).cards.size + cardsPerPosition._2.size > 5 => false
      case Middle if playerDeck.deck(Middle).cards.size + cardsPerPosition._2.size > 5 => false
      case _ => true
    }
/*
    playerDeck.deck map {
      case (Top, stack) if stack.cards.size + cardsPerPosition._2 == 3 =>
      case (Middle, stack) =>
      case (Bottom, stack) =>
    }
*/
  }

  import language.postfixOps
  import scala.concurrent.duration._

  /* try reconnecting until successful */
  def tryReconnect = {
    def doTry(attempts: Int): Unit = {
      context.system.actorSelection("akka.tcp://OpenFaceChinesePoker@127.0.0.1:1234/user/lobby")
        .resolveOne(10 seconds)
        .onComplete {
          case Success(ref) =>
            log.info("Server found, attempting to connect...")
            server = ref
            server ! Messages.Server.Connect(me)
          case Failure(exception) =>
            log.info(s"No game server found, retrying (${attempts+1})...")
            Thread.sleep(5000)
            doTry(attempts+1)
        }
    }

    log.info("Attempting to find a game server...")
    context become connecting
    doTry(0)
  }

  override def preStart(): Unit = {
    log.info("Welcome to Open Face Chinese Poker! Please enter your name.")
    Utils.readResponse.onComplete {
      case Success(name: String) =>
        me = Player(Utils.uuid(), name)
        tryReconnect
      case _ => self ! PoisonPill
    }
  }
}