package com.tomo.game.server

import akka.actor.{ActorSystem, Props}
import com.tomo.game.server.actor.{GameSupervisorActor, LobbyActor}

object Server {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("OpenFaceChinesePokerServer")
    system.actorOf(LobbyActor.props, "lobby")
  }

}
