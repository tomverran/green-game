package io.tvc.greengame

import cats.data.{NonEmptyList, ReaderT, StateT, WriterT}
import cats.instances.vector._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Monad}
import io.tvc.greengame.AI.Player

import scala.language.higherKinds

object Game {

  type GameState[F[_], A] = StateT[F, Board, A]
  type BoardReader[F[_], A] = ReaderT[F, Board, A]

  object GameState {
    def unit[F[_] : Applicative]: GameState[F, Unit] = StateT.pure[F, Board, Unit](())
  }

  case class Log(board: Board, players: Vector[Player])
  implicit class BoardReaderSyntax[F[_] : Applicative, A](br: BoardReader[F, A]) {

    /**
      * Convert a function that only needs knowledge of the current state of the board
      * into one that technically has write access to the board
      */
    def asState: GameState[F, A] = StateT[F, Board, A](s => br.run(s).map(s -> _))
  }

  /**
    * Decide whether the game has finished or not
    * Will return a left of GameFinished if we're out of forecasts
    * or a Right of players if the game should continue with the current player state
    */
  def checkGameFinished[F[_]](players: Vector[Player])(implicit F: Applicative[F]): GameState[F, Either[Vector[Player], Log]] =
    StateT { board =>
      board.forecasts.tail match {
        case Nil => F.pure(board -> Right(Log(board, players)))
        case h +: t => F.pure(board.copy(forecasts = NonEmptyList(h, t)) -> Left(players))
      }
    }

  /**
    * Run the actual game, should be stack safe due to use of trampolining
    * Repeatedly calls playRound until we're finished
    */
  def runGame[F[_] : Monad : Random](ai: AI[F], players: Vector[Player]): GameState[F, Vector[Log]] = {

    type GameStateF[A] = GameState[F, A]
    type BoardReaderF[A] = BoardReader[F, A]
    type GameLog[A] = WriterT[GameStateF, Vector[Log], A]
    def lift[A](v: GameStateF[A]): GameLog[A] = WriterT.liftF[GameStateF, Vector[Log], A](v)

    def playRound(players: Vector[Player]): GameStateF[Vector[Player]] = for {
      withCards <- players.traverse(ai.pickCardToPlay).asState
      scored <- Scoring.applyScores[F](withCards)
      pickedCards <- scored.traverse(ai.chooseMarketCard)
      _ <- Market.replenishMarket[F]
    } yield pickedCards

    Monad[GameLog].tailRecM(players) { players =>
      for {
        board <- lift(StateT.get)
        _ <- WriterT.tell[GameStateF, Vector[Log]](Vector(Log(board, players)))
        players <- lift(playRound(players))
        result <- lift(checkGameFinished[F](players))
      } yield result
    }.run.map { case (log, last) => log :+ last }
  }
}
