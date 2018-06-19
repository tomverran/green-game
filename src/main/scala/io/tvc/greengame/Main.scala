package io.tvc.greengame
import cats.syntax.show._
import ShowInstances._
import cats.Id

object Main extends App {
  val (board, players) = Board.setup
  println(show"$board $players")

  val (finalBoard, finished) = Game.runGame[Id](AI.fairlySensibleAI, players).run(board)
  println(show"$finalBoard ${finished.finalPlayers}")
}
