package io.tvc.greengame

import Scoring._
import cats.Id
import io.tvc.greengame.AI.PlayerWithCard
import org.scalatest.{Matchers, WordSpec}

class ScoringTest extends WordSpec with Matchers {

  val (board, players) = Board.setup[Id]

  "Scoring" when {

    "Calling applyScores" should {

      val withCard = players.map(p => PlayerWithCard(p.copy(hand = p.hand.tail), card = Some(p.hand.head)))
      val (newBoard, withScore) = applyScores[Id](withCard).run(board)

      "Give out the right number of tokens" in {
        withScore.head.tokens shouldEqual score(board.forecasts.head)(withCard.head.card.get)
      }

      "Place the player's cards into the discard pile" in {
        newBoard.discard shouldEqual withCard.flatMap(_.card.toVector)
      }

      "Order the list of people by how many green tokens they earned" in {
        pending
      }
    }
  }
}
