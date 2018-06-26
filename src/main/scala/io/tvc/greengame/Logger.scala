package io.tvc.greengame

import cats.{Applicative, Id, Monad, Show}
import cats.data.{StateT, WriterT}
import io.tvc.greengame.AI.{Player, PlayerWithCard}
import io.tvc.greengame.Game.GameState
import io.tvc.greengame.Logger.Event
import cats.syntax.show._
import cats.instances.string._
import cats.instances.int._
import ShowInstances._
import io.tvc.greengame.Market.CostedCard

import scala.language.higherKinds

/**
  * An event which occurred during the game
  * Which can be useful for debugging AI
  */
trait Logger[F[_]] {
  def log(event: Event): F[Unit]
}

object Logger {

  sealed trait Event
  case class RoundBegin(board: Board, players: Vector[Player]) extends Event
  case class GameEnd(board: Board, players: Vector[Player]) extends Event
  case class CardPlayed(player: PlayerWithCard) extends Event
  case class CardPurchased(player: Player, card: CostedCard) extends Event
  type EventLog = Vector[Event]

  implicit val showEvent: Show[Event] = {
    case RoundBegin(board, players) => show"Round begin: $board $players"
    case GameEnd(board, players) => show"Game Finished: $board $players"
    case CardPlayed(p) => show"${p.player.name} played ${p.card}"
    case CardPurchased(p, c) => show"${p.name} purchased ${c.card} for ${c.cost}"
  }

  implicit val idLogger: Logger[Id] =
    Function.const(())

  implicit def writerTLogger[F[_] : Monad]: Logger[WriterT[F, EventLog, ?]] =
    (event: Event) => WriterT.tell[F, EventLog](Vector(event))

  def logCardPlayed[F[_] : Logger : Applicative](p: PlayerWithCard): GameState[F, Unit] =
    logGame[F](_ => CardPlayed(p))

  def logPurchase[F[_] : Logger : Applicative](p: Player, c: CostedCard): GameState[F, Unit] =
    logGame[F](_ => CardPurchased(p, c))

  def logGame[F[_] : Logger : Applicative](event: Board => Event): GameState[F, Unit] =
    StateT.inspectF[F, Board, Unit](b => implicitly[Logger[F]].log(event(b)))
}
