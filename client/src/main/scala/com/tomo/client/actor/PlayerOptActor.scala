package com.tomo.client.actor

import akka.actor.{Actor, ActorRef, DiagnosticActorLogging, PoisonPill, Props}
import akka.pattern.pipe
import com.tomo.client.actor.PlayerOptActor.PlayerState
import main.scala.com.tomo.common.domain._
import main.scala.com.tomo.common.{Messages, Utils}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Failure, Success}

object PlayerOptActor {

  case class PlayerState(phase: Phase,
                         playerHand: CardStack,
                         playerDroppedCard: Option[Card],
                         playerBoard: PlayerBoard,
                         playerMoves: Map[Position, List[Card]],
                         originalPlayerHand: CardStack,
                         originalBoard: PlayerBoard)

  def props = Props(new PlayerOptActor)
}

class PlayerOptActor extends Actor with DiagnosticActorLogging {
  implicit val executionContext: ExecutionContext = context.dispatcher

  /* the server currently joined */
  var server: ActorRef = null

  /* the gameRoom currently joined */
  var gameRoom: GameRoom = null

  /* actor reference to the currently joined gameRoom */
  var gameRoomRef: ActorRef = null

  /* the player */
  var me: Player = null

  /* the opponents */
  var opponents: List[Player] = List.empty

  var myState = PlayerOptActor.PlayerState(NotPlaying, CardStack.empty, None, PlayerBoard.empty, Map.empty, CardStack.empty, PlayerBoard.empty)

  /* all players decks */
  var visibleDecks: Map[Player, PlayerBoard] = null

  val emptyMoves: Map[Position, List[Card]] = Map(Top -> List.empty, Middle -> List.empty, Bottom -> List.empty, DroppedCard -> List.empty)

  /* reset the game state. This is usually called before restarting the game */
  def resetState = {
    gameRoom = null
    opponents = List.empty
    visibleDecks = Map.empty
  }

  /* leave the room and reset the game state */
  def leaveRoom= {
    gameRoomRef ! Messages.Game.Leave
    resetState
    gameRoom = null
    gameRoomRef = null
  }

  /* the player is waiting to connect to a game lobby */
  def connecting: Receive = {
    case Messages.Server.Connected =>
      log.info("Connected to the server, waiting for opponents...")
      context become waiting
  }

  def waiting: Receive = {
    case Messages.Game.Joined(room) =>
      gameRoom = room
      gameRoomRef = sender

      log.info(s"You have joined the game room: ${gameRoom.name}")
      context become waitingToStart
  }

  def waitingToStart: Receive = {
    case Messages.Game.SetUp(players) =>
      opponents = players
      val playersDecks = players filterNot(_ == me) map (_ -> PlayerBoard.empty)
      visibleDecks = playersDecks.toMap ++ Map(me -> PlayerBoard.empty)
      log.info(s"You are playing against ${opponents.filterNot(_ == me).mkString(", ")}")
      val newInitialPlayerState = PlayerOptActor.PlayerState(FirstDraw, CardStack.empty, None, PlayerBoard.empty, Map(Top -> List.empty, Middle -> List.empty, Bottom -> List.empty), CardStack.empty, PlayerBoard.empty)
      context become drawing(newInitialPlayerState)
  }

  def drawing(playerState: PlayerOptActor.PlayerState): Receive = {
    case Messages.Game.SendHand(hand, phase) =>
      val newPlayerState = PlayerOptActor.PlayerState(phase, hand, None, playerState.playerBoard, emptyMoves, hand, myState.playerBoard)
      context become play(newPlayerState)
      val userInstructions =
        s""" ============ $phase ============
           | $hand
           | ===================================
           |             Play Time
           | Please send your moves as follow:
           |   - T=indexesTop
           |   - M=indexesMiddle
           |   - B=indexesBottom
           |   - D=indexDropDeck
           | Each indexes corresponds to the
           | index of the card in your hand:
           |""".stripMargin
      val cardsWithIndex = hand.cards.zipWithIndex
      log.info(userInstructions)
      cardsWithIndex foreach(cardIndex => log.info(s"${cardIndex._2} - ${cardIndex._1}"))
      visibleDecks(me).positionCardStack.foreachEntry((position, stack) => log.info(s"$position = [$stack]"))
      log.info("===================================")

    case Messages.Game.NotYourTurn(player) =>
      log.info(s"You already played")
  }

  def play(playerState: PlayerOptActor.PlayerState): Receive = {
    case Messages.Game.Terminate(reason) => reason match {
      case reason: Messages.Game.Terminate.Reason.PlayerLeft if reason.player != me =>
        log.info("Your opponents have left the game, you will be sent back to the lobby")
        leaveRoom
      case reason: Messages.Game.Terminate.Reason.ErrorOccured =>
        log.info(s"The game terminated due to an error, you will be sent back to the lobby. Reason: ${reason.error}")
        leaveRoom
    }
    case Messages.Game.DrawTime =>
      val newPlayerState = PlayerOptActor.PlayerState(playerState.phase, CardStack.empty, None, playerState.playerBoard, emptyMoves, CardStack.empty, playerState.originalBoard)
      context become drawing(newPlayerState)
    case Messages.Game.ScoreTime =>
      context become scoring

    case Messages.Game.NotYourTurn(player) =>
      log.info(s"Please wait your turn, it's ${player.name}'turn.")
      context become notPlaying(playerState)
    case Messages.Game.PlayerTurn(phase) =>
      log.info(s"[$phase] It's your turn.")
    case Messages.Game.PlayerTurnEnded =>
      log.info("Your turn is over. Please wait for the others to play")
      context become notPlaying(playerState)

    case Messages.Game.MovesAccepted =>
      log.info("Your moves were accepted by Server")
      log.info(s"previous was: ${playerState.originalBoard}")
      log.info(s"now is: ${playerState.playerBoard}")
      val newPlayerState = PlayerOptActor.PlayerState(playerState.phase, CardStack.empty, playerState.playerDroppedCard, playerState.playerBoard, emptyMoves, playerState.originalPlayerHand, playerState.playerBoard)
      visibleDecks += (me -> playerState.playerBoard)
      context become notPlaying(newPlayerState)
    case Messages.Game.InvalidInputs(phase, reason, _) =>
      val newPlayerState = PlayerOptActor.PlayerState(phase, playerState.originalPlayerHand, None, playerState.originalBoard, emptyMoves, playerState.originalPlayerHand, playerState.originalBoard)
      log.info(s"Your moves were invalidated by Server: $reason\n$newPlayerState")
      context become play(newPlayerState)
      self ! Messages.Player.AskMoves(phase)
    case Messages.Player.PlayerInvalidInput(reason) =>
      log.info(s"Your input was invalid: $reason")
      self ! Messages.Player.AskMoves(playerState.phase)

    case Messages.Player.ConfirmMoves(phase) =>
      log.info(s"Do yo confirm your move?\n${playerState.playerBoard}")
      val playerInputFuture = StdIn.readLine()
      Future(playerInputFuture)
        .map(Utils.readYesNo(_) match {
          case Some(yes) =>
            if (yes) {
              val playerMoves = playerState.playerMoves
              log.info(s"Moves => ${playerMoves.toString()}")
              log.info(s"board => ${playerState.playerBoard}")
              log.info(s"original => ${playerState.originalBoard}")
              val newPlayerState = PlayerOptActor.PlayerState(playerState.phase, playerState.playerHand, playerState.playerDroppedCard, playerState.playerBoard, playerMoves, playerState.originalPlayerHand, playerState.originalBoard)
              context become play(newPlayerState)
              gameRoomRef ! Messages.Player.PlayerCompleteMoves(playerMoves)
            }
            else {
              log.info("Please re-enter your moves")
              val newPlayerState = PlayerOptActor.PlayerState(phase, playerState.originalPlayerHand, None, playerState.originalBoard, emptyMoves, playerState.originalPlayerHand, playerState.originalBoard)
              context become play(newPlayerState)
              self ! Messages.Player.AskMoves(phase)
            }
          case None =>
            log.info("Didn't understand...")
            self ! Messages.Player.ConfirmMoves(phase)
        })

    case Messages.Game.AskMoves(phase) =>
      self ! Messages.Player.AskMoves(phase)

    case Messages.Player.AskMoves(phase) =>
      log.info(s"=========== $phase =============")
      val myHand = playerState.playerHand
      val handWithIndex = myHand.cards.zipWithIndex
      playerState.playerBoard.positionCardStack foreachEntry((position, cards) => log.info(s"$position = [$cards]"))
      log.info("===================================")
      handWithIndex foreach(cardIndex => log.info(s"${cardIndex._2} - ${cardIndex._1}"))

      val playerInputFuture = StdIn.readLine()
      Future(playerInputFuture)
        .map(Utils.readMove(_, myHand.cards) match {
          case Some(cardsPerPosition) =>
            val playedPosition = cardsPerPosition._1
            val playedCards = cardsPerPosition._2
            val playerHand = playerState.playerHand
            val playerBoard = playerState.playerBoard
            val newPlayerHand = CardStack(playerState.playerHand.cards diff playedCards)
            val newPlayerMoves = playerState.playerMoves + (playedPosition -> (playerState.playerMoves(playedPosition) ++ playedCards))
            log.info(s"played: ${playedCards.mkString} in $playedPosition")
            log.info(s"new Hand: $newPlayerHand")
            log.info(s"new PlayerMoves: $newPlayerMoves")
            if (isValidMove(phase, cardsPerPosition, playerHand, playerBoard, playerState.playerDroppedCard.isDefined)) {
              if (!alreadyDropped(playerState.playerDroppedCard, playedPosition, playedCards)) {
                val newDroppedCard = Some(playedCards.head)
                val newPlayerState = PlayerOptActor.PlayerState(phase, newPlayerHand, newDroppedCard, playerBoard, newPlayerMoves, playerState.originalPlayerHand, playerState.originalBoard)
                context become play(newPlayerState)
                log.info(s"Dropped Cards: ${newDroppedCard.get}")
              }

              val newBoardPosition = playerBoard.positionCardStack(playedPosition).cards ++ playedCards
              val newPlayerBoard = PlayerBoard(playerBoard.positionCardStack + (playedPosition -> newBoardPosition))
              val newPlayerState = PlayerOptActor.PlayerState(phase, newPlayerHand, playerState.playerDroppedCard, newPlayerBoard, newPlayerMoves, playerState.originalPlayerHand, playerState.originalBoard)
              context become play(newPlayerState)
              log.info(s"$newPlayerState")

              if (newPlayerHand.cards.isEmpty) {
                log.info(s"Sending moves : ${playerState.playerMoves}")
                Messages.Player.ConfirmMoves(phase)
              }
              else {
                log.info("Still have some cards in hand")
                Messages.Player.AskMoves(phase)
              }
            }
            else {
              log.info(s"error: $playerState")
              val msg = phase match {
                case FirstDraw if playedPosition == DroppedCard && playedCards.nonEmpty =>
                  "no card can be dropped in First Draw"
                case _ =>
                  if (newPlayerHand.isEmpty && playerState.playerDroppedCard.isEmpty)
                    "no card was dropped this turn"
                  else if (newPlayerMoves(playedPosition).size + playerState.playerBoard.positionCardStack(playedPosition).cards.size > playedPosition.nbCards)
                    s"too much cards: $playedPosition: ${playedCards.mkString}"
                  else if (!playedCards.forall(playerState.playerHand.cards.contains(_)))
                    "trying to play cards not in hand"
                  else
                    "unknown..."
              }
              Messages.Player.PlayerInvalidInput(msg)
            }
          case None =>
            val msg = "cannot read your inputs"
            Messages.Player.PlayerInvalidInput(msg)
          })
        .recover{ e =>
          val msg = "recover" + e.getMessage
          log.info(s"Error Recover: $msg")
          Messages.Player.PlayerInvalidInput(msg)
        }
        .pipeTo(self)
  }

  def notPlaying(playerState: PlayerOptActor.PlayerState): Receive = {
    case Messages.Game.PlayerTurn(phase) =>
      log.info(s"[$phase] It's your turn........")
      val newInitialPlayerState = PlayerOptActor.PlayerState(phase, playerState.playerHand, playerState.playerDroppedCard, playerState.playerBoard, playerState.playerMoves, playerState.originalPlayerHand, playerState.originalBoard)
      context become play(newInitialPlayerState)
    case Messages.Game.UpdateGameState(allVisibleDeck, lastPlayer) =>
      log.info(s"${lastPlayer.name} has just played. This is the updated board: $allVisibleDeck")
      visibleDecks = allVisibleDeck
    case Messages.Game.DrawTime =>
      context become drawing(playerState)
    case Messages.Game.AskMoves(_) =>
      log.info(s"This is not your turn")
    case Messages.Game.NotYourTurn(player) =>
      log.info(s"This is ${player.name} turn")
  }

  def playing: Receive = {
    case Messages.Game.Lost =>
      log.info("You lost the game...")
      context become finished
    case Messages.Game.Win =>
      log.info("You won the game !")
      context become finished
    case t: Messages.Game.Terminate => t.reason match {
      case reason: Messages.Game.Terminate.Reason.PlayerLeft if reason.player != me =>
        log.info("Your opponents have left the game, you will be sent back to the lobby")
        leaveRoom
      case reason: Messages.Game.Terminate.Reason.ErrorOccured =>
        log.info(s"The game terminated due to an error, you will be sent back to the lobby. Reason: ${reason.error}")
        leaveRoom
    }
    case Messages.Server.Message(msg) => log.info(s"[Server]: $msg")
  }

  def finished: Receive = playing orElse {
    case Messages.Game.Restart => {
      log.info("Opponent accepted rematch, starting a new round")
      resetState
      context become waitingToStart
    }
  }

  def scoring: Receive = {
    case _ =>
      log.info("Game ended => calculating score")
  }

  override def receive: Receive = connecting

  def alreadyDropped(playerDroppedCardOption: Option[Card], playedPosition: Position, playedCards: List[Card]): Boolean =
    !(playerDroppedCardOption.isEmpty && playedCards.size == 1 && playedPosition == DroppedCard)

  def isValidMove(phase: Phase, cardsPerPosition: (Position, List[Card]), playerHand: CardStack, playerBoard: PlayerBoard, isDroppedCardDefined: Boolean): Boolean = {
    val playedPosition = cardsPerPosition._1
    val playedCards = cardsPerPosition._2
    val newPlayerHand = playerHand.cards diff playedCards

    val allCardsFromHand = playedCards.forall(playerHand.cards.contains(_))
    val respectPositionMaxCards = playedCards.size + playerBoard.positionCardStack(playedPosition).cards.size <= playedPosition.nbCards
    phase match {
      case FirstDraw => !isDroppedCardDefined && playedPosition != DroppedCard
      case _ =>
        if(playedPosition == DroppedCard)
          !isDroppedCardDefined && playedCards.size == 1 && allCardsFromHand && respectPositionMaxCards
        else {
          if(isDroppedCardDefined)
            playedPosition != DroppedCard && allCardsFromHand && respectPositionMaxCards
          else
            !newPlayerHand.isEmpty && allCardsFromHand && respectPositionMaxCards
        }
    }
  }

  import language.postfixOps
  import scala.concurrent.duration._

  /* try reconnecting until successful */
  def tryReconnect = {
    def doTry(attempts: Int): Unit = {
      context.system.actorSelection("akka.tcp://OpenFaceChinesePoker@127.0.0.1:1234/user/lobby")
        .resolveOne(10 seconds)
        .onComplete {
          case Success(ref) =>
            log.info("Server found, attempting to connect...")
            server = ref
            server ! Messages.Server.Connect(me)
          case Failure(exception) =>
            log.info(s"No game server found, retrying (${attempts+1})...")
            Thread.sleep(5000)
            doTry(attempts+1)
        }
    }

    log.info("Attempting to find a game server...")
    context become connecting
    doTry(0)
  }

  override def preStart(): Unit = {
    log.info("Welcome to Open Face Chinese Poker! Please enter your name.")
    Utils.readResponse.onComplete {
      case Success(name: String) =>
        me = Player(Utils.uuid(), name)
        tryReconnect
      case _ => self ! PoisonPill
    }
  }
}