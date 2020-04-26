package main.scala.com.tomo.common

import main.scala.com.tomo.common.domain._

object Messages {
  object Server {
    case class Connect(player: Player)
    case class Connected() extends Serializable
    case class Message(message: String) extends Serializable
  }

  object Player {
    case class Accept()
    case class Refuse()
    trait Interaction
    case class PlayerMoves(positionMove: (Position, List[Card])) extends Interaction
    case object PlayerInvalidInput extends Interaction
  }

  object Game {
    case class Joined(room: GameRoom)
    case class GiveCard(card: Card, phase: Phase, nbCardGiven: Int)
    case class AskMoves(phase: Phase)
    case object DrawTime
    case object PlayTime
    case class DrawTime(phase: Phase, deck: List[Card], playersSession: List[PlayerSession])
    case class SetUp(opponents: List[Player])
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

  object Score {
    case class BoardPointWinner(winner: Option[PlayerSession], players: (PlayerSession, PlayerSession))
    case class GetBoardPointWinner(toSeq: Seq[(Seq[(PlayerSession, (Int, Int))], Seq[(PlayerSession, (Int,Int))])])
    case class ComputeRoyalties(toSeq: Seq[((PlayerSession, Int), (PlayerSession, Int))])
    case class RoyaltiesScore()
  }
}
