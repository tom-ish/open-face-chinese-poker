package com.tomo.game

import akka.actor.ActorRef
import com.tomo.game.domain.{Card, CardStack, GameRoom, Phase, Player, PlayerCardStack, PlayerState}
import com.tomo.game.server.PlayerSession
import spray.json.DefaultJsonProtocol._
/*

case class PlayerState(playerName: String)
object PlayerState {
  implicit val jsonFormat = jsonFormat1(PlayerState.apply)
}

case class GameEvent(playerName: String, score: Int)
object GameEvent {
  implicit val jsonFormat = jsonFormat2(GameEvent.apply)
}

case class PlayerJoined(actor: ActorRef)

*/




object Messages {
  object Server {
    case class Connect(player: Player)
    case class Connected()
    case class Message(message: String)
  }

  object Player {
    case class Accept()
    case class Refuse()
  }

  object Game {
    case class Joined(room: GameRoom)
    case class GiveCard(card: Card, phase: Phase, nbCardGiven: Int)
    case class AskMoves(phase: Phase)
    case object DrawTime
    case object PlayTime
    case class DrawTime(phase: Phase, deck: List[Card], playersSession: List[PlayerSession])
    case class SetUp(opponents: List[Player])
    case class InTurn(top: Option[Card], state: PlayerState, opponentsState: List[PlayerState])
    case object ComputeScore
    case object Win
    case object Lost
    case object Restart
    case object Leave

    object Terminate {
      trait Reason

      object Reason {
        case class PlayerLeft(player: Player)
        case class ErrorOccured(error: Throwable)
      }

      def apply(p: Player): Reason.PlayerLeft = Reason.PlayerLeft(p)
      def apply(t: Throwable): Reason.ErrorOccured = Reason.ErrorOccured(t)
    }
    case class Terminate(reason: Terminate.Reason)
  }
}