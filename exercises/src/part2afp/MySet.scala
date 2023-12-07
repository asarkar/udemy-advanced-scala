package part2afp

trait MySet[A] extends (A => Boolean):
  override def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(other: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def -(elem: A): MySet[A]
  def &(other: MySet[A]): MySet[A]
  def --(other: MySet[A]): MySet[A]

  def unary_! : MySet[A]

  def toScalaSet: Set[A]
// EmptySet is a class not an object because MySet is invariant, not covariant in A.
class EmptySet[A] extends MySet[A]:
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet(elem, this)

  override def ++(other: MySet[A]): MySet[A] = other

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def toScalaSet: Set[A] = Set.empty[A]

  override def -(elem: A): MySet[A] = this

  override def &(other: MySet[A]): MySet[A] = this

  override def --(other: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A]:

  override def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  override def +(elem: A): MySet[A] = if contains(elem) then this else new NonEmptySet(elem, this)

  /*
   [1,2,3] ++ [4,5]
   = [2,3] ++ [4,5] + 1
   = ([3] ++ [4,5] + 2) + 1
   = (([] ++ [4,5] + 3) + 2) + 1
   = (([4,5] + 3) + 2) + 1
   = ([3,4,5] + 2) + 1
   = [2,3,4,5] + 1
   = [1,2,3,4,5]
   */
  override def ++(other: MySet[A]): MySet[A] = tail ++ other + head

  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] =
    val xs = tail.filter(predicate)
    if predicate(head) then xs + head else xs

  override def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)

  override def toScalaSet: Set[A] = tail.toScalaSet + head

  override def -(elem: A): MySet[A] = filter(x => x != elem)

  // filter(x => other.contains(x)) = filter(x => other.apply(x)) = filter(x => other(x)) = filter(other)
  override def &(other: MySet[A]): MySet[A] = filter(other)

  override def --(other: MySet[A]): MySet[A] = filter(!other)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !contains(x))

// all elements of type A which satisfy a property
// { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]:
  def contains(elem: A): Boolean = property(elem)
  // { x in A | property(x) } + element = { x in A | property(x) || x == element }
  def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)

  // { x in A | property(x) } ++ set => { x in A | property(x) || set contains x }
  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  /*
   We can't implement map and flatMap because we don't have a property B => Boolean
   to test the membership of the elements in the resulting set.
   However, foreach doesn't do any transformation of type, only side-effect.
   It seems to me foreach should be able to just apply f on the members of the set.
   */
  def map[B](f: A => B): MySet[B]            = ???
  def flatMap[B](f: A => MySet[B]): MySet[B] = ???
  def foreach(f: A => Unit): Unit            = ???

  def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))
  def -(elem: A): MySet[A]                      = filter(x => x != elem)
  def --(anotherSet: MySet[A]): MySet[A]        = filter(!anotherSet)
  def &(anotherSet: MySet[A]): MySet[A]         = filter(anotherSet)
  def unary_! : MySet[A]                        = new PropertyBasedSet[A](x => !property(x))

  override def toScalaSet: Set[A] = ???

object MySet:
  def apply[A](values: A*): MySet[A] =
    if values.isEmpty then new EmptySet[A] else values.foldRight(MySet[A]())((elem, acc) => acc + elem)
