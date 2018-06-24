package io.tvc.greengame
import cats.effect.IO
import cats.syntax.show._
import io.tvc.greengame.ShowInstances._
import io.tvc.greengame.Shuffler.syncShuffler

object Main extends App {
  (
    for {
      (board, players) <- Board.setup[IO]
      (finalBoard, finalPlayers) <- Game.runGame[IO](AI.fairlySensibleAI, players).run(board)
      unit <- IO(println(show"$finalBoard " + show"${finalPlayers.finalPlayers}"))
    } yield unit
  ).unsafeRunSync
}
