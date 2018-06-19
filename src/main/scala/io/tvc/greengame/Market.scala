package io.tvc.greengame
import cats.{Applicative, Show}
import cats.data.StateT
import io.tvc.greengame.AI.Player
import io.tvc.greengame.Game.GameState
import io.tvc.greengame.Market.CostedCard
import cats.syntax.applicative._
import cats.syntax.show._
import cats.instances.int._

import scala.language.higherKinds

case class Market(cards: Set[CostedCard]) {
  def remove(card: CostedCard) = Market(cards - card)
  def findAffordable(maxCost: Int): Set[CostedCard] = cards.filter(_.cost <= maxCost)
  def asMap: Map[Int, Card] = cards.map(c => c.cost -> c.card).toMap
}

object Market {

  val costs = List(0, 1, 1, 2, 2, 3, 3)
  case class CostedCard(cost: Int, card: Card)

  implicit val costedCardShow: Show[CostedCard] =
    c => show"£${c.cost}: ${c.card}"

  /**
    * Given a board with a potentially depleted market, attempt to replenish it
    * by taking cards from the unused cards deck. Bad things will happen if you run out of cards
    */
  def replenishMarket[F[_] : Applicative]: GameState[F, Unit] =
    StateT.modify[F, Board] { board =>
      val (newMarket, newDeck) = costs.foldLeft(board.market.asMap -> board.unusedCards) {
        case ((marketSoFar, deckSoFar), cost) =>
          (marketSoFar.get(cost), deckSoFar) match {
            case (None, h :: t) => marketSoFar.updated(cost, h) -> t
            case (_, _) => marketSoFar -> deckSoFar
          }
      }

      board.copy(
        market = Market(newMarket.map { case (cost, card) => CostedCard(cost, card) }.toSet),
        unusedCards = newDeck
      )
    }

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
