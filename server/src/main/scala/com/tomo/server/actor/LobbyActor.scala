package com.tomo.server.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import main.scala.com.tomo.common.{Messages, Utils}
import main.scala.com.tomo.common.domain.{GameRoom, GameSession, Player, PlayerSession}
import main.scala.com.tomo.game.server.actor.GameSupervisorActor

object LobbyActor {
  def props = Props(new LobbyActor)
}
class LobbyActor extends Actor with ActorLogging {
  var games: Map[ActorRef, GameSession] = Map.empty
  var players: Map[ActorRef, PlayerSession] = Map.empty

  var waitingPlayers: List[PlayerSession] = List.empty

  def tryMakeMatch = waitingPlayers.length match {
    case x if x == 2 || x == 3 =>
      val gameRoom = GameRoom(name = Utils.uuid())
      val gameRoomRef = context.system.actorOf(GameSupervisorActor.props(gameRoom), "game-supervisor")

      val playersMatched = waitingPlayers.take(x)
      waitingPlayers = waitingPlayers.drop(x)

      println(s"Players ${playersMatched.mkString(", ")} are matched and will promptly join the room $gameRoom")
      gameRoomRef ! GameSupervisorActor.Messages.ReceivePlayers(playersMatched)
    case x if x <= 1 => println("Not enough player to play Open Face Chinese Poker")
    case _ => println("Too much player to play Open Face Chinese Poker")
  }

  override def receive: Receive = {
    case Messages.Server.Connect(player: Player) =>
      println(s"Player connected:  $player")
      sender ! Messages.Server.Connected

      context.watch(sender)

      val session = PlayerSession(player, sender)
      players = players + (sender -> session)
      waitingPlayers = waitingPlayers :+ session

      tryMakeMatch

    case Terminated(ref: ActorRef) if players.isDefinedAt(ref) =>
      // a player has disconnected
      val session = players(ref)

      // remove player from the player list and the waiting list
      players = players - ref
      waitingPlayers = Utils.removeLast(waitingPlayers, session)

      println(s"Player has disconnected: ${session.player}")
  }

  override def preStart(): Unit = {
    println("Server is now ready to accept connections")
  }
}
