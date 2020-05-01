package com.tomo.server

import akka.actor.ActorSystem
import com.tomo.server.actor.LobbyOptActor

object ServerOpt {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("OpenFaceChinesePoker")
    system.actorOf(LobbyOptActor.props, "lobby")
  }

}
