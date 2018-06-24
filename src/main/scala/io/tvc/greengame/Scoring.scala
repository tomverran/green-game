package io.tvc.greengame

import cats.Applicative
import io.tvc.greengame.AI.{Player, PlayerWithCard}
import io.tvc.greengame.Game.BoardReader
import cats.data.ReaderT
import io.tvc.greengame.Scoring.Token.Green
import io.tvc.greengame.Weather.Forecast
import cats.syntax.applicative._

import scala.language.higherKinds

object Scoring {

  sealed trait Token extends Product with Serializable

  object Token {

    case object Black extends Token
    case object Green extends Token

    def green(times: Int): List[Token] =
      (0 until times).map(_ => Green).toList

    def black(times: Int): List[Token] =
      (0 until times).map(_ => Black).toList
  }

  /**
    * Find out what a single card would score given the forecast
    */
  def score(forecast: Forecast)(card: Card): List[Token] =
    (
      (card, forecast.weather) match {
        case (Card.Solar(value), Weather.Sunny) => Token.green(value * 2)
        case (Card.Tidal(value), Weather.Windy) => Token.green(value * 2)
        case (Card.Wind(value), Weather.Windy) => Token.green(value * 2)
        case (Card.Coal(value), _) => Token.black(value)
        case _ => Token.green(card.value)
      }
    ).take(forecast.demand)

  /**
    * Given a bunch of players who have just chosen cards, apply scores to them
    * then order the list by the most sustainable player
    */
  def applyScores[F[_]](playerWithCard: Vector[PlayerWithCard])(implicit F: Applicative[F]): BoardReader[F, Vector[Player]] =
    ReaderT { board =>
      playerWithCard.map {
        case PlayerWithCard(pl, None) => pl -> List.empty[Token]
        case PlayerWithCard(pl, Some(card)) => pl -> score(board.forecasts.head)(card)
      }
      .sortBy { case (_, tokens) => tokens.count(_ == Green) * -1 }
      .map { case (pl, tokens) => pl.copy(tokens = pl.tokens ++ tokens) }
      .pure[F]
    }
}
