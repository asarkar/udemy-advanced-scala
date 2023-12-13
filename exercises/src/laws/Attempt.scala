package laws

import cats.Monad
import scala.annotation.tailrec

sealed trait Attempt[+A]
case class Success[+A](value: A) extends Attempt[A]
case class Failure(e: Throwable) extends Attempt[Nothing]

object Attempt:
  given Monad[Attempt] with
    def flatMap[A, B](fa: Attempt[A])(f: A => Attempt[B]): Attempt[B] = fa match
      case Success(x) => f(x)
      case Failure(e) => Failure(e)

    def pure[A](a: A): Attempt[A] = Success(a)

    // Cats requires implementing tailRecM which encodes stack safe monadic recursion.
    @tailrec
    def tailRecM[A, B](a: A)(f: A => Attempt[Either[A, B]]): Attempt[B] = f(a) match
      case Success(Left(nextA)) => tailRecM(nextA)(f) // continue the recursion
      case Success(Right(b))    => pure(b)            // recursion done
      case Failure(e)           => Failure(e)
