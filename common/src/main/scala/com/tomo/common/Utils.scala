package main.scala.com.tomo.common

import java.util.UUID

import main.scala.com.tomo.common.domain.{Bottom, Card, DroppedCard, Middle, Position, Top}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Utils {

  def removeLast[A](list: List[A], item: A): List[A] = {
    def remove(iter: List[A]): List[A] = iter match {
      case x :: y => {
        if(x == item) y
        else x :: remove(y)
      }
      case Nil => Nil
    }

    remove(list.reverse).reverse
  }

  def uuid(): String = UUID.randomUUID().toString


  def readResponse: Future[String] = Future {
    scala.io.StdIn.readLine()
  }

  def readYesNo(input: String): Option[Boolean] = input match {
    case "Y" => Some(true)
    case "N" => Some(false)
    case _ => None
  }

  def readMove(input: String, playerCards: List[Card]): Option[(Position, List[Card])] = {
    val p = input split "="
    val moves = p(1)
    val cardsIndexes = moves split ","
    val resultingRowDeck = p(0) match {
      case "T" =>
        getCardsInPosition(Top, cardsIndexes, playerCards)
      case "M" =>
        getCardsInPosition(Middle, cardsIndexes, playerCards)
      case "B" =>
        getCardsInPosition(Bottom, cardsIndexes, playerCards)
      case "D" =>
        getCardsInPosition(DroppedCard, cardsIndexes, playerCards)
    }
    resultingRowDeck
  }

  private def getCardsInPosition(position: Position, cardsIndexes: Array[String],
                                 playerCards: List[Card]): Option[(Position, List[Card])] = {
    if (position != DroppedCard && cardsIndexes.length > position.nbCards) None
    else if (!cardsIndexes.forall(_.length == 1)) None // if input string is more than 1 character
    else if (!cardsIndexes.forall(_.toInt <= 5)) None  // if input integer is more than 5
    else if (cardsIndexes.isEmpty) Some(position -> List.empty) // if input is P=empty => considering empty position deck
    else {
      val cards = cardsIndexes.map(i => playerCards(i.toInt))
      Some(position, cards.toList)
    }

  }
}
