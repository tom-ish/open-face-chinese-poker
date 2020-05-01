package com.tomo.client

import akka.actor.ActorSystem
import com.tomo.client.actor.PlayerOptActor

object ClientOpt {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("OpenFaceChinesePokerClient")
    system.actorOf(PlayerOptActor.props, "lobby")
  }
}
