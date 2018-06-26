package io.tvc.greengame

import cats.Id
import org.scalatest.{Inspectors, Matchers, WordSpec}

class BoardTest extends WordSpec with Matchers with Inspectors {

  // an initial state board for testing
  val (board, players) = Board.setup[Id]

  "Board setup function" should {

    "Produce three players with three cards each and no tokens" in {
      players.length shouldBe 3
      forAll(players)(_.hand.length shouldBe 3)
      forAll(players)(_.tokens.all shouldBe 0)
    }

    "Produce a weather forecast of 7 days with reasonable demands" in {
      forAll(board.forecasts.toList)(_.demand shouldBe Random[Id].nextInt(6))
      board.forecasts.length shouldBe 7
    }

    "Include all the possible weathers in the forecasts" in {
      // because Random[Id].shuffle does nothing we know the weather
      val expected = Weather.all ++ Weather.all :+ Weather.all.head
      board.forecasts.map(_.weather).toList.toVector shouldEqual expected
    }

    "Include 60 total resource cards" in {
      (board.unusedCards ++ players.flatMap(_.hand) ++ board.market.cards.map(_.card)).length shouldBe 60
    }

    "Include 12 total weather cards" in {
      (board.unusedWeather ++ board.forecasts.map(_.weather).toList).length shouldBe 12
    }
  }

  "Board takeCard function" should {

    "Do nothing when zero cards are requested" in {
      val (newBoard, cards) = Board.takeCards[Id](0).run(board)
      cards shouldEqual Vector.empty
      newBoard shouldEqual board
    }

    "Take cards from the unused resources where available" in {
      val (newBoard, cards) = Board.takeCards[Id](3).run(board)
      newBoard.unusedCards shouldEqual board.unusedCards.drop(3)
      cards shouldEqual board.unusedCards.take(3)
    }

    "Shuffle the discard pile into unusedCards when there are not enough unused to take" in {
      val depleted = board.copy(unusedCards = board.unusedCards.take(1), discard = board.unusedCards.tail)
      val (newBoard, cards) = Board.takeCards[Id](3).run(depleted)
      cards shouldEqual depleted.unusedCards ++ depleted.discard.take(2)
      newBoard.unusedCards shouldEqual depleted.discard.drop(2)
    }
  }
}
