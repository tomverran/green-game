package io.tvc.greengame
import cats.Show

import scala.util.Random._
import cats.data.NonEmptyList
import io.tvc.greengame.AI.Player
import io.tvc.greengame.Market.CostedCard
import io.tvc.greengame.Weather.Forecast
import cats.instances.int.catsStdShowForInt
import cats.syntax.show._
import ShowInstances._

case class Board(
  forecasts: NonEmptyList[Weather.Forecast],
  market: Market,
  unusedWeather: List[Weather],
  unusedCards: List[Card],
  discard: List[Card]
)

object Board {

  implicit val boardShow: Show[Board] = { board =>
    show"""
       |Balance The Grid
       |-----------------
       | - Forecasts: ${board.forecasts}
       | - Market: ${board.market.cards.toList.sortBy(_.cost)}
       | - Unused weather cards remaining: ${board.unusedWeather.length}
       | - Unused market cards remaining: ${board.unusedCards.length}
     """.stripMargin
  }

  /**
    * Create a fresh, well laminated board
    * with everything set up in an initial state
    */
  def setup: (Board, List[Player]) = {

    val names = shuffle(List("Greg", "Hope", "Tom", "Michael", "Erica", "Josh"))
    val deck = shuffle(Weather.all ++ Weather.all ++ Weather.all ++ Weather.all)
    val (weather, remainingWeather) = deck.splitAt(7)

    val resourceDeck = shuffle((1 to 10).map(_ => Card.all).toList.flatten)
    val (market, forPlayers) = resourceDeck.splitAt(8)

    val (playerCards, remainingCards) = forPlayers.splitAt(9)
    val players = playerCards.grouped(3).toList.zip(names).map { case (cards, name) =>
      Player(name, cards, tokens = List.empty)
    }

    Board(
      forecasts = NonEmptyList.fromListUnsafe(weather.map(w => Forecast(w, nextInt(6) + 1))),
      market = Market(Market.costs.zip(market).map { case (cost, card) => CostedCard(cost, card) }.toSet),
      unusedWeather = remainingWeather,
      unusedCards = remainingCards,
      discard = List.empty
    ) -> players
  }
}

