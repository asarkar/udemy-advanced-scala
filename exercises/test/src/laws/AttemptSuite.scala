package laws

import org.scalatest.funsuite.AnyFunSuite
// There's also FlatSpecDiscipline, FunSpecDiscipline, and WordSpecDiscipline
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{Arbitrary, Gen}
import cats.Eq
import cats.laws.discipline.MonadTests

class AttemptSuite extends AnyFunSuite with FunSuiteDiscipline with Checkers:
  given attemptArb[A: Arbitrary]: Arbitrary[Attempt[A]] = Arbitrary(
    Gen.oneOf(
      Gen.const(Failure(RuntimeException())),
      (for {
        a <- Arbitrary.arbitrary[A]
      } yield Success(a))
    )
  )

  given attemptEq[A: Eq]: Eq[Attempt[A]] = Eq.fromUniversalEquals

  checkAll(
    "Attempt should satisfy Monad properties",
    MonadTests[Attempt].monad[Int, Int, Int]
  )
