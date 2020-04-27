package main.scala.com.tomo.common

import java.util.UUID

import main.scala.com.tomo.common.domain.{Bottom, Card, Middle, Position, Top}

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
    }
    resultingRowDeck
  }

  private def getCardsInPosition(position: Position, cardsIndexes: Array[String],
                                 playerCards: List[Card]): Option[(Position, List[Card])] = {
    if (cardsIndexes.length > position.nbCards) None
    else if (!cardsIndexes.forall(_.length == 1)) None
    else if (!cardsIndexes.forall(_.toInt <= 5)) None
    else if (cardsIndexes.isEmpty) Some(position -> List.empty)
    else {
      val cards = cardsIndexes.map(i => playerCards(i.toInt))
      Some(position, cards.toList)
    }

  }
}
