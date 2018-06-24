package io.tvc.greengame
import scala.language.higherKinds
import scala.util.Random
import cats.Id
import cats.effect.Sync

trait Shuffler[F[_]] {
  def shuffle[A](v: Vector[A]): F[Vector[A]]
}

object Shuffler {

  implicit class ShuffleSyntax[F[_] : Shuffler, A](v: Vector[A])  {
    def shuffle: F[Vector[A]] = implicitly[Shuffler[F]].shuffle(v)
  }

  implicit def idShuffler[F[_]]: Shuffler[Id] = new Shuffler[Id] {
    def shuffle[A](v: Vector[A]): Id[Vector[A]] = v
  }

  implicit def syncShuffler[F[_] : Sync]: Shuffler[F] = new Shuffler[F] {
    override def shuffle[A](v: Vector[A]): F[Vector[A]] = Sync[F].delay(Random.shuffle(v))
  }
}
