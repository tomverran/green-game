package io.tvc.greengame

import cats.data.{ReaderT, StateT}
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import cats.{Monad, Show}
import io.tvc.greengame.AI.{Player, PlayerWithCard}
import io.tvc.greengame.Game.{BoardReader, GameState}
import io.tvc.greengame.Market.CostedCard
import io.tvc.greengame.Scoring.Token.{Black, Green}
import io.tvc.greengame.Scoring._
import io.tvc.greengame.ShowInstances._

import scala.language.higherKinds

/**
  * Algebra for AI behaviours
  * Not sure if this is right
  */
trait AI[F[_]] {
  def pickCardToPlay(player: Player): BoardReader[F, PlayerWithCard]
  def chooseMarketCard(player: Player): GameState[F, Player]
}

object AI {

  case class Player(name: String, hand: Vector[Card], tokens: Vector[Token])
  case class PlayerWithCard(player: Player, card: Option[Card])

  implicit val playerShow: Show[Player] =
    pl => show"""
       |Name: ${pl.name}
       |Hand: ${pl.hand}
       |Green: ${pl.tokens.count(_ == Green)}
       |Black: ${pl.tokens.count(_ == Black)}
     """.stripMargin

  def fairlySensibleAI[F[_]](implicit F: Monad[F]): AI[F] = new AI[F] {
    case class CardWithRoi(roi: Int, costedCard: CostedCard)

    /**
      * Choose a card to play
      * This fairly sensible AI player will pick the best card they have
      * for the current forecast, but without looking into the future
      */
    def pickCardToPlay(player: Player): BoardReader[F, PlayerWithCard] =
      ReaderT { board =>
        player.hand.sortBy(score(board.forecasts.head)(_).length * -1) match {
          case chosen +: others => F.pure(PlayerWithCard(player.copy(hand = others), Some(chosen)))
          case Vector() => F.pure(PlayerWithCard(player, None))
        }
      }

    /**
      * Choose a card from the market
      * Arguably the trickiest part - we choose a card based on the best ROI in future rounds
      */
    def chooseMarketCard(player: Player): GameState[F, Player] =
      StateT.inspect[F, Board, Option[CostedCard]] { board =>

        val upcoming = board.forecasts.tail
        val affordable = board.market.findAffordable(player.tokens.length)

        val bestPerRound: List[CardWithRoi] = upcoming.flatMap { fc =>
          val forecastEstimate = fc.copy(demand = 4) // we're not supposed to know the demand so go with an optimistic estimate
          val returns = affordable.map(card => CardWithRoi(score(forecastEstimate)(card.card).length - card.cost, card))
          val bestRoi = returns.sortBy(_.roi * -1).headOption
          bestRoi
        }

        bestPerRound
          .groupBy(_.costedCard).toList
          .sortBy { case (_, rois) => rois.maxBy(_.roi).roi * rois.length }.headOption.map(_._1)

      }.flatMap { bestCard =>
        bestCard.fold[GameState[F, Player]](StateT.pure(player))(Market.purchase(player))
      }
  }
}
