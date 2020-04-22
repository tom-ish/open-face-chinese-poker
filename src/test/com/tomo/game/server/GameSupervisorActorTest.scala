package com.tomo.game.server

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameSupervisorActorTest extends TestKit(ActorSystem("gameActor")) with ImplicitSender
  with AnyWordSpec with Matchers with BeforeAndAfterAll {



  "A LobbyActor" should {
    "create a new PlayerSession" in {
      import com.tomo.game.Messages
      import com.tomo.game.domain.Player
      import com.tomo.game.server.actor.LobbyActor

      val lobbyTestActor = TestActorRef(LobbyActor.props)
      val player = Player("player1", "Player#1")
      lobbyTestActor !  Messages.Server.Connect(player)
      expectMsg(Messages.Server.Connect(player))
    }
  }

  it should {
    "distribute to 2 players X cards" in {
      import com.tomo.game.domain.{CardStack, Player}
      import com.tomo.game.server.actor.{GameSupervisorActor, LobbyActor}

      val lobbyActor = LobbyActor.props
      val gameSupervisorActor = GameSupervisorActor.props()

      val baseDeck = CardStack.sorted
      val p1 = Player("player1", "Player#1")
      val p2 = Player("player2", "Player#2")

      val p3 = Player("player3", "Player#3")
    }
  }

  override protected def afterAll(): Unit = system.terminate()
}
