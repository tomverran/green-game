package io.tvc.greengame
import cats.data.WriterT
import cats.effect.IO
import cats.syntax.show._
import io.tvc.greengame.ShowInstances._
import cats.syntax.traverse._
import cats.instances.vector._
import io.tvc.greengame.Logger.EventLog
import io.tvc.greengame.Random.syncRandom
import io.tvc.greengame.ShowInstances._

object Main extends App {
  (
    for {
      (board, players) <- Board.setup[WriterT[IO, EventLog, ?]]
      finished <- Game.runGame[WriterT[IO, EventLog, ?]](AI.fairlySensibleAI, players).runA(board)
      _ <- WriterT.tell[IO, EventLog](Vector(finished))
    } yield ()
  ).written
   .flatMap(_.traverse[IO, Unit](l => IO(println(show"$l"))))
   .unsafeRunSync
}
