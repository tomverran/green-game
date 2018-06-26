package io.tvc.greengame
import cats.data.{NonEmptyList, StateT}
import cats.instances.int.catsStdShowForInt
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.{Applicative, Functor, Monad, Show}
import io.tvc.greengame.AI.Player
import io.tvc.greengame.Game.GameState
import io.tvc.greengame.Market.CostedCard
import io.tvc.greengame.Random._
import io.tvc.greengame.ShowInstances._
import io.tvc.greengame.Weather.Forecast
import cats.syntax.applicative._
import io.tvc.greengame.Scoring.Tokens

import scala.language.higherKinds

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
       | - Discard pile size: ${board.discard.length}
     """.stripMargin
  }

  private def replenishUnused[F[_] : Random : Functor](b: Board): F[Board] =
    b.discard.shuffle.map(discard => b.copy(discard = Vector.empty, unusedCards = b.unusedCards ++ discard))

  /**
    * Obtain the given number of unused cards from the unused pile,
    * if there aren't enough then fewer will be returned
    */
  private def takeUnused[F[_] : Applicative](number: Int): GameState[F, Vector[Card]] =
    StateT { board =>
      (
        board.copy(unusedCards = board.unusedCards.drop(number)),
        board.unusedCards.take(number)
      ).pure[F]
    }


  /**
    * Obtain the given number of resource cards
    * from either the unused resource pile or the discard pile
    */
  def takeCards[F[_] : Monad : Random](number: Int): GameState[F, Vector[Card]] =
    StateT { board =>
      val b: F[Board] = if (board.unusedCards.length < number) replenishUnused[F](board) else board.pure[F]
      b.flatMap(b => takeUnused[F](number).run(b))
    }

  /**
    * Create a fresh, well laminated board
    * with everything set up in an initial state
    */
  def setup[F[_] : Random : Monad]: F[(Board, Vector[Player])] =
    for {
      names <- Vector("Greg", "Hope", "Tom", "Michael", "Erica", "Josh").shuffle
      weatherDeck <- (Weather.all ++ Weather.all ++ Weather.all ++ Weather.all).shuffle
      resourceDeck <- (0 until 5).map(_ => Card.all).toVector.flatten.shuffle
      demands <- (0 until 7).toVector.traverse(_ => Random[F].nextInt(6).map(_ + 1))
    } yield {

      val (weather, remainingWeather) = weatherDeck.splitAt(7)
      val (market, forPlayers) = resourceDeck.splitAt(7)
      val (playerCards, remainingCards) = forPlayers.splitAt(9)
      val players = createPlayers(names.take(3), playerCards)
      val forecasts = weather.zip(demands).map { case (w, d) => Forecast(w, d) }

      Board(
        forecasts = NonEmptyList.fromListUnsafe(forecasts.toList),
        market = Market(Market.costs.zip(market).map { case (cost, card) => CostedCard(cost, card) }),
        unusedWeather = remainingWeather,
        unusedCards = remainingCards,
        discard = Vector.empty
      ) -> players
    }

  /**
    * Given a bunch of player names and 3 cards, each, allocate them to each player
    */
  def createPlayers(names: Vector[String], playerCards: Vector[Card]): Vector[Player] =
    playerCards.grouped(3).toVector.zip(names).map { case (cards, name) =>
      Player(name, cards, tokens = Tokens.tokensMonoid.empty)
    }
}

