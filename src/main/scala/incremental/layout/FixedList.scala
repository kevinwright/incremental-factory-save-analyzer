package incremental.layout

import scala.compiletime.ops.int.*

class FixedList[N <: Int, +A] private (
    protected val inner: List[A]
) extends Iterable[A] {
  type WidthT = N

  override protected[this] def className: String = "FixedList"
  override def iterator: Iterator[A] = inner.iterator

  override def map[B](fn: A => B): FixedList[N, B] =
    new FixedList[N, B](inner.map(fn))

  def flatMap[NB <: Int, B](fn: A => FixedList[NB, B]): FixedList[N * NB, B] =
    new FixedList[N * NB, B](inner.flatMap(x => fn(x).inner))

  def zip[B](other: FixedList[N, B]): FixedList[N, (A,B)] =
    new FixedList[N, (A,B)](inner.zip(other))
}

object FixedList {
  class UnsafeFromList[N <: Int] {
    def apply[T](list: List[T])(using N: ValueOf[N]): FixedList[N, T] =
      val expected = N.value
      val actual = list.size
      assert(
        actual == expected,
        s"FixedList.unsafeFromList expected a list of size {$expected}, got {$actual}"
      )
      new FixedList[N, T](list)
  }

  def unsafeFromList[N <: Int & Singleton] = new UnsafeFromList[N]()

  def of[
      Tup <: Tuple,
      N <: Tuple.Size[Tup] & Singleton
  ](
      tup: Tup
  ): FixedList[N, Tuple.Union[Tup]] = {
    val inner = tup.toList.asInstanceOf[List[Tuple.Union[Tup]]]
    new FixedList[N, Tuple.Union[Tup]](inner)
  }

  def of[T](t: T): FixedList[1, T] = {
    new FixedList[1, T](List(t))
  }

  def empty: FixedList[0, Nothing] = new FixedList[0, Nothing](Nil)
}
