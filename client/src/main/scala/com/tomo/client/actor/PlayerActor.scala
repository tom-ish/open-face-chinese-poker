package com.tomo.client.actor

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import main.scala.com.tomo.common.{Messages, Utils}
import main.scala.com.tomo.common.domain.{Card, CardStack, GameRoom, Player, PlayerDeck}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

case class ServerFound(ref: ActorRef)
case class CardsPlayed(cards: List[Card])

object PlayerActor {
  def props = Props(new PlayerActor)
}

class PlayerActor extends Actor with ActorLogging {
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
      println("Connected to the server, waiting for opponents...")
      context become waiting
  }

  def waiting: Receive = {
    case Messages.Game.Joined(room) =>
      gameRoom = room
      gameRoomRef = sender

      println(s"You have joined the game room: ${gameRoom.name}")
      context become waitingToStart
  }

  def waitingToStart: Receive = {
    case Messages.Game.SetUp(players) =>
      opponents = players
      println(s"You are playing against ${opponents.filterNot(_ == me).mkString(", ")}")
      context become drawing
  }

  def drawing: Receive = {
    case Messages.Game.GiveCard(card, phase, i) =>
      if (phase.nbCard == i) {
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
        println(userInstructions)
        cardsWithIndex.foreach(cardIndex => println(s"${cardIndex._2} - ${cardIndex._1}\n"))
        println("===================================")
        context become play
      }
      else
        myHand = CardStack(myHand.cards ++ List(card))
  }

  def play: Receive = {
    case Messages.Game.Terminate(reason) => reason match {
      case reason: Messages.Game.Terminate.Reason.PlayerLeft if reason.player != me =>
        println("Your opponents have left the game, you will be sent back to the lobby")
        leaveRoom
      case reason: Messages.Game.Terminate.Reason.ErrorOccured =>
        println(s"The game terminated due to an error, you will be sent back to the lobby. Reason: ${reason.error}")
        leaveRoom
    }
    case Messages.Game.AskMoves(phase) =>
      println(s"[$phase] It's your turn.")
      Utils.readResponse.onComplete {
        case Success(input: String) =>
          // TODO: Parse received string into PlayerDeck <=> Map[Position, CardStack]
          val movePosition = Utils.readMove(input, myHand.cards)
          movePosition match {
            case Some(move) => Messages.Player.PlayerMoves(move)
            case None => sender ! Messages.Player.PlayerInvalidInput
          }
          context become drawing
        case _ => self ! PoisonPill
      }

  }

  def playing: Receive = {
    case Messages.Game.Lost =>
      println("You lost the game...")
      context become finished
    case Messages.Game.Win =>
      println("You won the game !")
      context become finished
    case t: Messages.Game.Terminate => t.reason match {
      case reason: Messages.Game.Terminate.Reason.PlayerLeft if reason.player != me =>
        println("Your opponents have left the game, you will be sent back to the lobby")
        leaveRoom
      case reason: Messages.Game.Terminate.Reason.ErrorOccured =>
        println(s"The game terminated due to an error, you will be sent back to the lobby. Reason: ${reason.error}")
        leaveRoom
    }
    case CardsPlayed => println("Please wait until it is your turn")
    case Messages.Server.Message(msg) => println(s"[Server]: $msg")
  }

  def finished: Receive = playing orElse {
    case Messages.Game.Restart => {
      println("Opponent accepted rematch, starting a new round")
      resetState
      context become waitingToStart
    }
  }

  override def receive: Receive = connecting

  import language.postfixOps
  import scala.concurrent.duration._

  /* try reconnecting until successful */
  def tryReconnect = {
    def doTry(attempts: Int): Unit = {
      context.system.actorSelection("akka.tcp://OpenFaceChinesePoker@127.0.0.1:1234/user/lobby")
        .resolveOne(10 seconds)
        .onComplete {
          case Success(ref) =>
            println("Server found, attempting to connect...")
            server = ref
            server ! Messages.Server.Connect(me)
          case Failure(exception) =>
            System.err.println(s"No game server found, retrying (${attempts+1})...")
            Thread.sleep(5000)
            doTry(attempts+1)
        }
    }

    println("Attempting to find a game server...")
    context become connecting
    doTry(0)
  }

  override def preStart(): Unit = {
    println("Welcome to Open Face Chinese Poker! Please enter your name.")
    Utils.readResponse.onComplete {
      case Success(name: String) =>
        me = Player(Utils.uuid(), name)
        tryReconnect
      case _ => self ! PoisonPill
    }
  }
}