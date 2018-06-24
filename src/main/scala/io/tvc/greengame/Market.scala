package io.tvc.greengame
import cats.{Applicative, Monad, Show}
import cats.data.StateT
import io.tvc.greengame.AI.Player
import io.tvc.greengame.Game.GameState
import io.tvc.greengame.Market.CostedCard
import cats.syntax.applicative._
import cats.syntax.show._
import cats.instances.int._

import scala.language.higherKinds

case class Market(cards: Vector[CostedCard]) {
  def remove(card: CostedCard) = Market(cards.filter(_ != card))
  def affordable(maxCost: Int): Vector[CostedCard] = cards.filter(_.cost <= maxCost)
}

object Market {

  val costs = Vector(0, 1, 1, 2, 2, 3, 3)
  case class CostedCard(cost: Int, card: Card)

  implicit val costedCardShow: Show[CostedCard] =
    c => show"£${c.cost}: ${c.card}"

  /**
    * Given a board with a potentially depleted market, attempt to replenish it
    * by taking cards from the unused cards deck. Bad things will happen if you run out of cards
    */
  def replenishMarket[F[_] : Monad : Random]: GameState[F, Unit] =
    for {
      board <- StateT.get[F, Board]
      oldCards = board.market.cards.sortBy(_.cost).map(_.card)
      newCards <- Board.takeCards[F](costs.length - oldCards.length)
      market = costs.zip(oldCards ++ newCards).map { case (cost, card) => CostedCard(cost, card) }
      updated <- StateT.set(board.copy(market = Market(market)))
    } yield updated

  /**
    * Purchase a card from the market
    * This function assumes the player can afford the card and that the card is in the market
    * but what can you do
    */
  def purchase[F[_] : Applicative](player: Player)(costedCard: CostedCard): GameState[F, Player] =
    StateT { board =>
      (
        board.copy(market = board.market.remove(costedCard)),
        player.copy(hand = player.hand :+ costedCard.card, tokens = player.tokens.drop(costedCard.cost))
      ).pure[F]
    }
}
