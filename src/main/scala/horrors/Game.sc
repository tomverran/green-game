import cats.data.{NonEmptyList, State}
import cats.instances.list._
import cats.syntax.all._

/**
  * This is the original friday night stream of consciousness
  * that ended up becoming quite complicated
  */

import scala.annotation.tailrec
import scala.util.Random.{nextInt, shuffle}
import scala.math.min

sealed trait Token
case object Black extends Token
case object Green extends Token


def green(times: Int): List[Token] =
  (0 until times).map(_ => Green).toList

def black(times: Int): List[Token] =
  (0 until times).map(_ => Green).toList

sealed trait Weather extends Product with Serializable

object Weather {

  case object Sunny extends Weather
  case object Windy extends Weather
  case object Cloudy extends Weather

  val all: List[Weather] = List(
    Sunny, Windy, Cloudy
  )
}

sealed abstract class Card extends Product with Serializable {
  def value: Int
}

object Card {

  case class Coal(value: Int) extends Card
  case class Solar(value: Int) extends Card
  case class Wind(value: Int) extends Card
  case class Tidal(value: Int) extends Card

  val all: List[Card] = List(
    Coal(1),  Coal(2),  Coal(3),
    Solar(1), Solar(2), Solar(3),
    Wind(1),  Wind(2),  Wind(3),
    Tidal(1), Tidal(2), Tidal(3)
  )
}

case class Forecast(weather: Weather, demand: Int)

def score(forecast: Forecast)(card: Card): List[Token] =
  (
    (card, forecast.weather) match {
      case (Card.Solar(value), Weather.Sunny) => green(value * 2)
      case (Card.Tidal(value), Weather.Windy) => green(value * 2)
      case (Card.Wind(value), Weather.Windy) => green(value * 2)
      case (Card.Coal(value), _) => black(value)
      case _ => green(card.value)
    }
  ).take(forecast.demand)

case class Player(
  hand: List[Card],
  tokens: List[Token]
)

case class CostedCard(cost: Int, card: Card)
case class CardWithRoi(roi: Int, costedCard: CostedCard)

val costs = List(0, 1, 1, 2, 2, 3, 3)

case class Board(
  forecasts: NonEmptyList[Forecast],
  market: List[CostedCard],
  unusedWeather: List[Weather],
  unusedCards: List[Card],
  discard: List[Card]
)

def setup: (Board, List[Player]) = {

  val deck = shuffle(Weather.all ++ Weather.all ++ Weather.all ++ Weather.all)
  val (weather, remainingWeather) = deck.splitAt(7)

  val cards = shuffle((1 to 10).map(_ => Card.all).toList.flatten)
  val (market, forPlayers) = cards.splitAt(7)

  val (playerCards, remainingCards) = forPlayers.splitAt(9)
  val players = playerCards.grouped(3).map(cards => Player(cards, tokens = List.empty)).toList

  Board(
    forecasts = NonEmptyList.fromListUnsafe(weather.map(w => Forecast(w, nextInt(6) + 1))),
    market = costs.zip(market).map { case (cost, card) => CostedCard(cost, card) },
    unusedWeather = remainingWeather,
    unusedCards = remainingCards,
    discard = List.empty
  ) -> players
}

def chooseCard(board: Board)(player: Player): (Player, Option[Card]) =
  player.hand.sortBy(score(board.forecasts.head)(_).length * -1) match {
    case chosen :: others => player.copy(hand = others) -> Some(chosen)
    case Nil => player -> None
  }


def allocateScores(player: Player, card: Option[Card], forecast: Forecast): Player =
  player.copy(tokens = player.tokens ++ card.fold(List.empty[Token])(score(forecast)))

def chooseBestMarketCard(player: Player, upcoming: List[Forecast], market: Set[CostedCard]): (Set[CostedCard], Player) = {
  val affordable = market.filter(_.cost <= player.tokens.length)

  val bestPerRound: List[CardWithRoi] = upcoming.flatMap { fc =>
    val forecastEstimate = fc.copy(demand = 4) // we're not supposed to know the demand so go with an optimistic estimate
    val returns = affordable.map(card => CardWithRoi(score(forecastEstimate)(card.card).length - card.cost, card))
    val bestRoi = returns.toList.sortBy(_.roi * -1).headOption
    bestRoi
  }

  val numRoundsCardIsBestFor = bestPerRound.groupBy(_.costedCard)
  val finalChoice = numRoundsCardIsBestFor.toList.sortBy { case (_, rois) => rois.maxBy(_.roi).roi * rois.length }.headOption

  finalChoice.fold(market -> player) { case (choice, _) =>
    market - choice -> player.copy(
      hand = player.hand :+ choice.card,
      tokens = player.tokens.sortBy(_ == Black).drop(choice.cost)
    )
  }
}

def repopulateMarket(deck: List[Card], market: List[CostedCard]): (List[CostedCard], List[Card]) = {
  val (newMarket, newDeck) = costs.foldLeft(market.map(m => m.cost -> m.card).toMap -> deck) {
    case ((marketSoFar, deckSoFar), cost) =>
      (marketSoFar.get(cost), deckSoFar) match {
        case (None, h :: t) => marketSoFar.updated(cost, h) -> t
        case (_, _) => marketSoFar -> deckSoFar
      }
  }
  (newMarket.map { case (cost, card) => CostedCard(cost, card) }.toList, newDeck)
}

def sortBySustainability(scored: Vector[(Player, List[Token])]) = {
  scored.sortBy { case (_, token) => token.map(t => if (t == Green) 1 else -1).sum }
}
@tailrec
def runGame(board: Board, players: List[Player]): (List[Player], Board) = {

  val forecast = board.forecasts.head
  val playerChoices = players.map(chooseCard(board)).toMap
  val scored = playerChoices.mapValues(_.fold(List.empty[Token])(score(forecast))).toVector
  println(scored)

  val bySustainability = sortBySustainability(scored)
  val scoresApplied = bySustainability.map { case (p, tokens) => p.copy(tokens = p.tokens ++ tokens) }

  val (depletedMarket, finalPlayers) = scoresApplied.foldLeft(board.market -> List.empty[Player]) {
    case ((market, pls), player) =>
      val (newMarket, pl) = chooseBestMarketCard(player, board.forecasts.tail, market.toSet)
      (newMarket.toList, pls :+ pl)
  }

  val (newMarket, newDeck) = repopulateMarket(board.unusedCards, depletedMarket)


  board.forecasts.tail match {
    case Nil => finalPlayers -> board
    case h :: t => runGame(
      board.copy(
        forecasts = NonEmptyList.of(h, t:_*),
        unusedCards = newDeck,
        market = newMarket
      ), finalPlayers)
  }
}

val (board, players) = setup
val (finalPlayers, finalBoard) = runGame(board, players)

finalPlayers.foreach(p => println(s"${p.tokens.length}"))

import cats.Id
import io.tvc.greengame
val (tvcBoard, tvcPlayers) = greengame.Board.setup
greengame.Game.runGame[Id](greengame.AI.fairlySensibleAI, tvcPlayers).run(tvcBoard)

