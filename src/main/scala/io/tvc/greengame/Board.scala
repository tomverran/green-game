package io.tvc.greengame
import cats.{Monad, Show}
import cats.data.NonEmptyList
import cats.instances.int.catsStdShowForInt
import cats.syntax.show._
import io.tvc.greengame.AI.Player
import io.tvc.greengame.Market.CostedCard
import io.tvc.greengame.ShowInstances._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.tvc.greengame.Weather.Forecast
import Shuffler._

import scala.language.higherKinds
import scala.util.Random._

case class Board(
  forecasts: NonEmptyList[Weather.Forecast],
  market: Market,
  unusedWeather: Vector[Weather],
  unusedCards: Vector[Card],
  discard: Vector[Card]
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
  def setup[F[_] : Shuffler : Monad]: F[(Board, Vector[Player])] =
    for {
      names <- Vector("Greg", "Hope", "Tom", "Michael", "Erica", "Josh").shuffle
      weatherDeck <- (Weather.all ++ Weather.all ++ Weather.all ++ Weather.all).shuffle
      resourceDeck <- (1 to 10).map(_ => Card.all).toVector.flatten.shuffle
    } yield {

      val (weather, remainingWeather) = weatherDeck.splitAt(7)
      val (market, forPlayers) = resourceDeck.splitAt(8)

      val (playerCards, remainingCards) = forPlayers.splitAt(9)
      val players = createPlayers(names, playerCards)

      Board(
        forecasts = NonEmptyList.fromListUnsafe(weather.map(w => Forecast(w, nextInt(6) + 1)).toList),
        market = Market(Market.costs.zip(market).map { case (cost, card) => CostedCard(cost, card) }),
        unusedWeather = remainingWeather,
        unusedCards = remainingCards,
        discard = Vector.empty
      ) -> players
    }

  def createPlayers(names: Vector[String], playerCards: Vector[Card]): Vector[Player] =
    playerCards.grouped(3).toVector.zip(names).map { case (cards, name) =>
      Player(name, cards, tokens = Vector.empty)
    }
}

