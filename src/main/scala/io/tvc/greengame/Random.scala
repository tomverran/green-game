package io.tvc.greengame
import scala.language.higherKinds
import scala.util.{Random => ScalaRandom}
import cats.Id
import cats.effect.Sync

trait Random[F[_]] {
  def shuffle[A](v: Vector[A]): F[Vector[A]]
  def nextInt(max: Int): F[Int]
}

object Random {

  def apply[F[_] : Random]: Random[F] = implicitly[Random[F]]

  implicit class RandomSyntax[F[_] : Random, A](v: Vector[A])  {
    def shuffle: F[Vector[A]] = implicitly[Random[F]].shuffle(v)
  }

  implicit def idRandom[F[_]]: Random[Id] = new Random[Id] {
    def shuffle[A](v: Vector[A]): Id[Vector[A]] = v
    def nextInt(max: Int): Int = max
  }

  implicit def syncRandom[F[_] : Sync]: Random[F] = new Random[F] {
    override def shuffle[A](v: Vector[A]): F[Vector[A]] = Sync[F].delay(ScalaRandom.shuffle(v))
    override def nextInt(max: Int): F[Int] = Sync[F].delay(ScalaRandom.nextInt(max))
  }
}
