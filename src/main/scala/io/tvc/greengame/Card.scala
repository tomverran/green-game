package io.tvc.greengame

import cats.Show
import cats.instances.int._
import cats.syntax.show._

sealed abstract class Card extends Product with Serializable {
  def value: Int
}

object Card {

  case class Coal(value: Int) extends Card
  case class Solar(value: Int) extends Card
  case class Wind(value: Int) extends Card
  case class Tidal(value: Int) extends Card

  implicit val showCard: Show[Card] = {
    case Coal(value) => show"Coal $value"
    case Solar(value) => show"Solar $value"
    case Wind(value) => show"Wind $value"
    case Tidal(value) => show"Tidal $value"
  }

  val all: List[Card] = List(
    Coal(1),  Coal(2),  Coal(3),
    Solar(1), Solar(2), Solar(3),
    Wind(1),  Wind(2),  Wind(3),
    Tidal(1), Tidal(2), Tidal(3)
  )
}

