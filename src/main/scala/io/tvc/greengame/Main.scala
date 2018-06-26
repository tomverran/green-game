package io.tvc.greengame
import cats.effect.IO
import cats.syntax.show._
import io.tvc.greengame.ShowInstances._
import io.tvc.greengame.Random.syncRandom
import cats.syntax.traverse._
import cats.instances.vector._
import AI.playerShow

object Main extends App {
  (
    for {
      (board, players) <- Board.setup[IO]
      log <- Game.runGame[IO](AI.fairlySensibleAI, players).runA(board)
      unit <- log.traverse[IO, Unit](l => IO(println(show"${l.board}" +vectorShow(playerShow).show(l.players))))
    } yield unit
  ).unsafeRunSync
}
