package main.scala.com.tomo.common.domain

import java.util.Date

case class GameRoom(val name: String) {
  val createAt = new Date()
}
