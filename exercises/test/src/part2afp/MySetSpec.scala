package part2afp

import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.util.Using
// For generator-driven + table-driven property checks, mix in trait ScalaCheckPropertyChecks.
class MySetSpec extends AnyFunSpec with ScalaCheckDrivenPropertyChecks:
  describe("A Set"):
    describe("when empty"):
      val emptySet = MySet[Int]()

      it("should contain nothing"):
        forAll { (n: Int) =>
          assert(!emptySet.contains(n) && !emptySet.toScalaSet.contains(n))
        }

      it("should produce a singleton set when an element is added"):
        forAll { (n: Int) =>
          val xs = emptySet + n
          assert(xs.toScalaSet == Set(n))
        }

      it("should add an element only once"):
        forAll { (n: Int) =>
          val xs = emptySet + n + n
          assert(xs.toScalaSet == Set(n))
        }

      it("should produce the other set when a set is added"):
        forAll { (other: Set[Int]) =>
          val xs = emptySet ++ MySet(other.toSeq*)
          assert(xs.toScalaSet == other)
        }

      it("should produce an empty set when mapped over"):
        assert(emptySet.map(_ * 2).toScalaSet == Set.empty)

      it("should produce an empty set when flatmapped over"):
        assert(emptySet.flatMap(x => MySet(x * 2)).toScalaSet == Set.empty)

      it("should produce an empty set when filtered"):
        assert(emptySet.filter(_ => true).toScalaSet == Set.empty)

      it("should have no side-effect on each element"):
        Using(new ByteArrayOutputStream()) { out =>
          Console.withOut(out):
            emptySet.foreach(println)
          assert(out.size() == 0)
        }

      it("should produce an empty set when an element is removed"):
        forAll { (n: Int) =>
          val xs = emptySet - n
          assert(xs.toScalaSet == Set.empty)
        }

      it("should produce an empty set when intersected with another set"):
        forAll { (xs: Set[Int]) =>
          val other = MySet[Int](xs.toSeq*)
          assert((emptySet & other).toScalaSet == Set.empty)
        }

      it("should produce an empty set upon taking the difference with another set"):
        forAll { (xs: Set[Int]) =>
          val other = MySet[Int](xs.toSeq*)
          assert((emptySet -- other).toScalaSet == Set.empty)
        }

      it("should produce the unary negation of itself"):
        forAll { (n: Int) =>
          assert((!emptySet).contains(n))
        }

    describe("when non-empty"):
      val nums: Gen[(MySet[Int], Set[Int])] = for
        xs <- Gen.containerOf[Set, Int](Gen.choose(-1000, 1000))
        set = MySet[Int](xs.toSeq*)
      yield (set, xs)

      it("should contain the given elements"):
        forAll(nums) { xs =>
          assert(xs._1.toScalaSet == xs._2)
        }

      it("should add an element only once"):
        forAll { (xs: Set[Int], n: Int) =>
          val set = MySet[Int](xs.toSeq*) + n + n
          assert(set.toScalaSet == xs + n)
        }

      it("should produce the union of itself with the set added"):
        forAll { (xs: Set[Int], other: Set[Int]) =>
          val set = MySet[Int](xs.toSeq*) ++ MySet(other.toSeq*)
          assert(set.toScalaSet == xs ++ other)
        }

      it("should produce a non-empty set when mapped over"):
        forAll(nums) { xs =>
          assert(xs._1.map(_ * 2).toScalaSet == xs._2.map(_ * 2))
        }

      it("should produce a non-empty set when flatmapped over"):
        forAll(nums) { xs =>
          assert(xs._1.flatMap(x => MySet(x * 2)).toScalaSet == xs._2.flatMap(x => Set(x * 2)))
        }

      it("should remove non-matching elements when filtered"):
        forAll(nums) { xs =>
          val even = (x: Int) => x % 2 == 0
          assert(xs._1.filter(even).toScalaSet == xs._2.filter(even))
        }

      it("should have the desired side-effect on each element"):
        forAll(nums) { xs =>
          Using(new ByteArrayOutputStream()) { out =>
            Console.withOut(out):
              xs._1.foreach(println)
            val lines: Array[String] = out.toString(StandardCharsets.UTF_8).split("\\s")
            assert(lines sameElements xs._2.map(_.toString).toArray)
          }
        }

      it("should remove an element"):
        forAll { (xs: Set[Int], n: Int) =>
          val set = MySet[Int](xs.toSeq*) + n - n
          assert(set.toScalaSet == xs - n)
        }

      it("should produce an intersection with another set"):
        forAll { (xs: Set[Int], other: Set[Int]) =>
          val set = MySet[Int](xs.toSeq*) & MySet(other.toSeq*)
          assert(set.toScalaSet == (xs intersect other))
        }

      it("should produce a difference with another set"):
        forAll { (xs: Set[Int], other: Set[Int]) =>
          val set = MySet[Int](xs.toSeq*) -- MySet(other.toSeq*)
          assert(set.toScalaSet == (xs diff other))
        }

      it("should produce the unary negation of itself"):
        forAll(nums) { xs =>
          whenever(xs._2.nonEmpty):
            val negative = !(xs._1)
            assert(xs._2.exists(!negative))
        }
