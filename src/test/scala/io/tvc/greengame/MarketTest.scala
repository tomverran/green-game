package io.tvc.greengame
import Market.replenishMarket
import Board.setup
import cats.Id
import org.scalatest.{Inspectors, Matchers, WordSpec}

class MarketTest extends WordSpec with Inspectors with Matchers {

  "Market replenish function" should {

    "Do nothing when the market is fully populated" in {
      pendingUntilFixed {
        info("replenishMarket needs re-implementation")
        val (board, _) = setup[Id]
        val replenished = replenishMarket[Id].runS(board)
        replenished.market shouldBe board.market
      }
    }
  }
}
