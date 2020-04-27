package com.tomo.server

import akka.actor.ActorSystem
import com.tomo.server.actor.LobbyActor

object Server {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("OpenFaceChinesePokerServer")
    system.actorOf(LobbyActor.props, "lobby")
  }

}
