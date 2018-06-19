package io.tvc.greengame

import cats.Show
import cats.syntax.show._
import cats.instances.int._

sealed trait Weather extends Product with Serializable

object Weather {

  case object Sunny extends Weather
  case object Windy extends Weather
  case object Cloudy extends Weather

  implicit val weatherShow: Show[Weather] =
    Show.fromToString[Weather]

  val all: List[Weather] = List(
    Sunny, Windy, Cloudy
  )

  case class Forecast(
    weather: Weather,
    demand: Int
  )

  implicit val forecastShow: Show[Forecast] =
    fc => show"${fc.weather} / ${fc.demand}"
}


