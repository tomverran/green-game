package io.tvc.greengame

import Scoring._
import cats.Id
import cats.data.NonEmptyList
import io.tvc.greengame.AI.PlayerWithCard
import io.tvc.greengame.Weather.{Forecast, Sunny}
import org.scalatest.{Inspectors, Matchers, WordSpec}

class ScoringTest extends WordSpec with Matchers with Inspectors {

  val (board, players) = Board.setup[Id]

  "Scoring" when {

    "Calling applyScores" should {

      val sun = NonEmptyList.of(Forecast(Sunny, 6))
      val cards = List(Card.Coal(1), Card.Wind(2), Card.Solar(3))
      val withCard = players.zip(cards).map { case (p, c) => PlayerWithCard(p, Some(c)) }
      val (newBoard, withScore) = applyScores[Id](withCard).run(board.copy(forecasts = sun))

      "Give out the right number of tokens" in {
        withScore.minBy(_.name).tokens shouldEqual score(sun.head)(withCard.minBy(_.player.name).card.get)
      }

      "Place the player's cards into the discard pile" in {
        newBoard.discard should contain allElementsOf withCard.flatMap(_.card.toVector)
      }

      "Order the list of people by how many green tokens they earned" in {
        forAll(withScore.zipWithIndex) {
          case (_, 0) => succeed
          case (score, i) => score.tokens.green should be <= withScore(i - 1).tokens.green
        }
      }
    }
  }
}
