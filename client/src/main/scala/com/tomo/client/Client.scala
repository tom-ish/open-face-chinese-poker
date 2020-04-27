package com.tomo.client

import akka.actor.ActorSystem
import com.tomo.client.actor.PlayerActor

object Client {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("OpenFaceChinesePokerClient")
    system.actorOf(PlayerActor.props, "lobby")
  }
}
