package io.tvc.greengame

import cats.Show
import cats.data.NonEmptyList

object ShowInstances {

  implicit def listShow[A : Show]: Show[List[A]] =
    list => list.map(Show[A].show).mkString(", ")

  implicit def vectorShow[A : Show]: Show[Vector[A]] =
    list => list.map(Show[A].show).mkString(", ")


  implicit def nelShow[A : Show]: Show[NonEmptyList[A]] =
    list => list.toList.map(Show[A].show).mkString(", ")
}
