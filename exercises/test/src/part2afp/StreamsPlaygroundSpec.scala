package part2afp

import org.scalatest.funspec.AnyFunSpec

class StreamsPlaygroundSpec extends AnyFunSpec:
  describe("A stream of natural numbers"):
    val naturals = MyStream.from(1)(_ + 1)

    it("should start with one"):
      assert(naturals.head == 1)

    it("should have two in the second position"):
      assert(naturals.tail.head == 2)

    it("should have three in the third position"):
      assert(naturals.tail.tail.head == 3)

    it("should be able to take the first 100000"):
      var xs = naturals.take(100000)
      var i  = 1

      while !xs.isEmpty && i <= 100000
      do
        assert(xs.head == i)
        i += 1
        xs = xs.tail

      assert(i == 100001)

    it("should be able to flatMap"):
      val xs = naturals.flatMap(x => Cons(x, Cons(x + 1, EmptyStream))).take(10).toList()
      val ys = List.range(1, 7)
      val zs = ys.zip(ys.tail).flatMap(_.toList)
      assert(xs == zs)

    it("should be able to filter and retain the first 10"):
      // If take(n) is more that 10, StackOverflowError, because
      // it'll keep looking for more elements that're not there.
      val xs = naturals.filter(_ <= 10).take(10).toList()
      assert(xs == List.range(1, 11).toList)

  describe("A zero prepended to a stream of natural numbers"):
    val startFrom0 = 0 #:: MyStream.from(1)(_ + 1) // naturals.#::(0)
    it("should be evaluated lazily"):
      assert(startFrom0.head == 0)

  describe("A stream of Fibonacci numbers"):
    it("should be evaluated lazily"):
      val fibs = StreamsPlayground.fibonacci(0, 1).take(20).toList()
      assert(fibs == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181))

  describe("A stream of Prime numbers"):
    it("should be evaluated lazily"):
      val primes      = StreamsPlayground.eratosthenes(MyStream.from(2)(_ + 1))
      val primesLt100 = primes.filter(_ < 100).take(25).toList()
      assert(
        primesLt100 == List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89,
          97)
      )
