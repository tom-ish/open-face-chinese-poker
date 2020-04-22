package com.tomo.game

import akka.actor.{ActorSystem, Props}
import akka.event.Logging
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, GraphDSL, Sink, Source}
import akka.util.Timeout
import spray.json._

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Failure
/*

class GameMultiplayer(implicit system: ActorSystem) {
  implicit val timeout = Timeout(5 seconds)

  val gameLogicFlow: Flow[PlayerState, GameEvent, _] =
    Flow.fromGraph(GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

  })

  val gameEventBroadcastFlow: Flow[GameEvent, GameEvent, _] = {
    val broadcastActor = system.actorOf(Props[BroadcasterActor], name = "broadcaster")
    val broadcastSink = Sink.actorRef(broadcastActor,
      onCompleteMessage = "finished",
      onFailureMessage = {_})

    val in = Flow[GameEvent].to(broadcastSink)
    val out = Source.actorRef[GameEvent](bufferSize = 1, OverflowStrategy.dropHead)
      .mapMaterializedValue(broadcastActor ! PlayerJoined(_))

    Flow.fromSinkAndSource(in, out)
  }
  val flow: Flow[Message, Message, _] = {
    Flow[Message]
      .log("incoming")(Logging(system, "MessageLogger"))
      .collect {
        case msg: TextMessage.Strict => msg.text
      }
      .map(_.parseJson.convertTo[PlayerState])
      .via(gameLogicFlow)
      .via(gameEventBroadcastFlow)
      .map(_.toJson.toString()
      .map(TextMessage.Strict)
      )
  }
}
*/
