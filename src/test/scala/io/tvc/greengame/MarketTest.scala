package io.tvc.greengame
import cats.Id
import io.tvc.greengame.Board.setup
import io.tvc.greengame.Market.{costs, replenishMarket}
import org.scalatest.{Inspectors, Matchers, WordSpec}

class MarketTest extends WordSpec with Inspectors with Matchers {

  val (board, _) = setup[Id]

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
}
