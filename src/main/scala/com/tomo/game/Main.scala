package com.tomo.game

import akka.actor.ActorSystem
import akka.event.slf4j.SLF4JLogging
import akka.http.scaladsl.Http

import scala.util.{Failure, Success}

/*
object Main extends App with SLF4JLogging {

  implicit val system = ActorSystem()

  val gameMultiplayer = new GameMultiplayer

  val route: Route = path("gameSocket") {
    get {
      handleWebSocketMessages()
    }
  }

  val config = system.settings.config
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")

  val serverBinding = Http().bindAndHandle(interface = interface, port = port, handler = route)

  serverBinding onComplete {
    case Success(binding) =>
      val localAddress = binding.localAddress
      log.info(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")

    case Failure(exception) =>
      log.error(s"Binding failed with ${exception.getMessage}")
      system.terminate()
  }
}
*/
