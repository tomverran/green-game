package io.tvc.greengame

import cats.data.{NonEmptyList, ReaderT, StateT}
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Monad}
import io.tvc.greengame.AI.Player

object Game {

  type GameState[F[_], A] = StateT[F, Board, A]
  type BoardReader[F[_], A] = ReaderT[F, Board, A]
  case class GameFinished(finalPlayers: List[Player])

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
  def checkGameFinished[F[_]](players: List[Player])(implicit F: Applicative[F]): GameState[F, Either[List[Player], GameFinished]] =
    StateT { board =>
      board.forecasts.tail match {
        case Nil => F.pure(board -> Right(GameFinished(players)))
        case h :: t => F.pure(board.copy(forecasts = NonEmptyList(h, t)) -> Left(players))
      }
    }

  /**
    * Run the actual game, should be stack safe due to use of trampolining
    * Repeatedly calls playRound until we're finished
    */
  def runGame[F[_] : Monad](ai: AI[F], players: List[Player]): GameState[F, GameFinished] = {

    type BoardReaderF[A] = BoardReader[F, A]
    type GameStateF[A] = GameState[F, A]

    def playRound(players: List[Player]): GameStateF[List[Player]] = for {
      withCards <- players.traverse(ai.pickCardToPlay).asState
      scored <- Scoring.applyScores[F](withCards).asState
      pickedCards <- scored.traverse(ai.chooseMarketCard)
      _ <- Market.replenishMarket[F]
    } yield pickedCards

    Monad[GameStateF].tailRecM(players) { players =>
      for {
        newBoard <- playRound(players)
        result <- checkGameFinished[F](newBoard)
      } yield result
    }
  }
}
