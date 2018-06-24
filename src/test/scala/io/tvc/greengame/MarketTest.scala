package io.tvc.greengame
import cats.Id
import io.tvc.greengame.Board.setup
import io.tvc.greengame.Market.{costs, replenishMarket}
import org.scalatest.{Inspectors, Matchers, WordSpec}

class MarketTest extends WordSpec with Inspectors with Matchers {

  val (board, _) = setup[Id]

  "Market replenish function" should {

    "Do nothing when the market is fully populated" in {
      val replenished = replenishMarket[Id].runS(board)
      replenished shouldBe board
    }

    "Build a new market from the unused cards when the market is empty" in {
      val replenished = replenishMarket[Id].runS(board.copy(market = Market(Vector.empty)))
      replenished.market.cards.map(_.card) shouldEqual board.unusedCards.take(7)
      replenished.market.cards.map(_.cost) shouldEqual costs
    }
  }
}
