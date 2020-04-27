package main.scala.com.tomo.common

import java.util.UUID

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
}
