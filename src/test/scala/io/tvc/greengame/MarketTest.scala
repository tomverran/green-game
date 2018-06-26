package io.tvc.greengame
import cats.Id
import io.tvc.greengame.AI.Player
import io.tvc.greengame.Board.setup
import io.tvc.greengame.Card.Coal
import io.tvc.greengame.Market.{CostedCard, costs, replenishMarket}
import io.tvc.greengame.Scoring.Tokens
import org.scalatest.{Inspectors, Matchers, WordSpec}

class MarketTest extends WordSpec with Inspectors with Matchers {

  val (board, player :+ _) = setup[Id]

  "Market replenish function" when {

    "the market is fully populated" should {
      val replenished = replenishMarket[Id].runS(board)

      "Do nothing" in {
        replenished shouldBe board
      }
    }

    "The market is empty" should {
      val replenished = replenishMarket[Id].runS(board.copy(market = Market(Vector.empty)))

      "Build a new market from the unused cards" in {
        replenished.market.cards.map(_.card) shouldEqual board.unusedCards.take(7)
        replenished.market.cards.map(_.cost) shouldEqual costs
      }
    }

    "the market is partially depleted" should {

      val depleted = board.market.cards.take(3).foldLeft(board.market) { case (m, c) => m.remove(c) }
      val replenished = replenishMarket[Id].runS(board.copy(market = depleted)).market.cards

      "Take the cards left in the market and pack them in order into the cheapest slots" in {
        replenished.take(4).map(_.card) shouldBe board.market.cards.slice(3, 7).map(_.card)
      }

      "Draw new cards from the unused card pile to make up the numbers" in {
        replenished.slice(4, 7).map(_.card) shouldBe board.unusedCards.take(3)
      }

      "Have all the right costs" in {
        replenished.map(_.cost) shouldBe Market.costs
      }
    }
  }

  "Market purchase function" should {

    val coalBoard = board.copy(market = Market(Vector(CostedCard(3, Coal(3)))))
    val coalPlayer = Player("Dougal Trimp", Vector.empty, Tokens(black = 2, green = 2))
    val (newBoard, player) = Market.purchase(coalPlayer)(coalBoard.market.cards.head).run(coalBoard)

    "Use black tokens to pay for cards before green ones" in {
      player.tokens shouldEqual Tokens(black = 0, green = 1)
    }

    "Remove the purchased card from the market" in {
      newBoard.market.cards.isEmpty shouldEqual true
    }
  }
}
