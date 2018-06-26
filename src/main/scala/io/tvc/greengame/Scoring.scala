package io.tvc.greengame

import cats.data.ReaderT
import cats.kernel.Monoid
import cats.syntax.applicative._
import cats.{Applicative, Monad}
import io.tvc.greengame.AI.{Player, PlayerWithCard}
import io.tvc.greengame.Game.{BoardReader, GameState, _}
import io.tvc.greengame.Weather.Forecast
import io.tvc.greengame.Scoring.Tokens._
import cats.syntax.semigroup._

import scala.language.higherKinds

object Scoring {

  case class Tokens(
    black: Int,
    green: Int
  ) {
    def all: Int = green + black
    def -(value: Int) = Tokens(math.max(0, black - value), green + math.min(0, black - value))
  }

  object Tokens {

    implicit val tokensMonoid: Monoid[Tokens] = new Monoid[Tokens] {
      def combine(x: Tokens, y: Tokens): Tokens = Tokens(x.black + y.black, x.green + y.green)
      def empty: Tokens = Tokens(black = 0, green = 0)
    }

    implicit val sorting: Ordering[Tokens] =
      Ordering.by(_.green * -1)

    def green(times: Int): Tokens = Tokens(black = 0, green = times)
    def black(times: Int): Tokens = Tokens(black = times, green = 0)
  }

  /**
    * Given some tokens, cap their black / green values to a particular maximum
    * Note this does not cap the overall amount so you've got to only have added one or the other
    */
  private def cap(to: Int)(tokens: Tokens): Tokens =
    Tokens(black = math.min(to, tokens.black), green = math.min(to, tokens.green))

  /**
    * Find out what a single card would score given the forecast
    */
  def score(forecast: Forecast)(card: Card): Tokens =
    cap(forecast.demand) (
      (card, forecast.weather) match {
        case (Card.Solar(value), Weather.Sunny) => Tokens.green(value * 2)
        case (Card.Tidal(value), Weather.Windy) => Tokens.green(value * 2)
        case (Card.Wind(value), Weather.Windy) => Tokens.green(value * 2)
        case (Card.Coal(value), _) => Tokens.black(value)
        case _ => Tokens.green(card.value)
      }
    )

  /**
    * Given a bunch of players who have just chosen cards, apply scores to them
    * then order the list by the most sustainable player
    */
  private def scores[F[_] : Applicative](playerWithCard: Vector[PlayerWithCard]): BoardReader[F, Vector[Player]] =
    ReaderT { board =>
      playerWithCard.map {
        case PlayerWithCard(pl, None) => pl -> Tokens.tokensMonoid.empty
        case PlayerWithCard(pl, Some(card)) => pl -> score(board.forecasts.head)(card)
      }
      .sortBy { case (_, tokens) => tokens }
      .map { case (pl, tokens) => pl.copy(tokens = pl.tokens |+| tokens) }
      .pure[F]
    }

  /**
    * Given a bunch of players with cards,
    * add all their cards into the discard pile
    */
  private def discardCards[F[_] : Applicative](playerWithCard: Vector[PlayerWithCard]): GameState[F, Unit] =
    playerWithCard.foldLeft(GameState.unit[F]) { case (state, PlayerWithCard(_, card)) =>
      state.modify(board => board.copy(discard = board.discard ++ card.toVector))
    }

  /**
    * Apply scores to players with cards, that is
    * add their cards to the discard pile and give them tokens
    */
  def applyScores[F[_] : Monad](players: Vector[PlayerWithCard]): GameState[F, Vector[Player]] =
    discardCards[F](players).flatMap(_ => scores[F](players).asState)
}
