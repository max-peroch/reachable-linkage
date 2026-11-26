package reachablelinkage.symbols

import scala.collection.mutable.ListBuffer

/** The few collection and string operations of Scala 2.13 and 3 that Scala 2.12 lacks, under names that exist in neither, so one source builds for both. */
private[reachablelinkage] object Collections {

  implicit final class EitherListOps[A, B](private val xs: List[Either[A, B]]) extends AnyVal {

    /** The lefts and the rights, each in order. */
    def partitionEithers: (List[A], List[B]) = {
      val lefts  = ListBuffer.empty[A]
      val rights = ListBuffer.empty[B]
      xs.foreach {
        case Left(a)  => lefts += a
        case Right(b) => rights += b
      }
      (lefts.toList, rights.toList)
    }
  }

  implicit final class ListOps[A](private val xs: List[A]) extends AnyVal {

    /** The first element of each distinct key, in order. */
    def distinctOn[K](key: A => K): List[A] = {
      val seen = scala.collection.mutable.HashSet.empty[K]
      xs.filter(a => seen.add(key(a)))
    }
  }

  implicit final class VectorOps[A](private val xs: Vector[A]) extends AnyVal {

    /** The values of each key, in order. */
    def groupMapOf[K, V](key: A => K)(value: A => V): Map[K, Vector[V]] =
      xs.groupBy(key).map { case (k, as) => k -> as.map(value) }

    /** The values of each key folded with `reduce`, in order. */
    def groupMapReduceOf[K, V](key: A => K)(value: A => V)(reduce: (V, V) => V): Map[K, V] =
      xs.groupBy(key).map { case (k, as) => k -> as.map(value).reduceLeft(reduce) }
  }

  implicit final class IteratorOps[A](private val it: Iterator[A]) extends AnyVal {

    /** The next element, if there is one. */
    def nextOpt: Option[A] = if (it.hasNext) Some(it.next()) else None
  }

  implicit final class IntIteratorOps(private val it: Iterator[Int]) extends AnyVal {

    /** The largest element, if there is one. */
    def maxOpt: Option[Int] = if (it.hasNext) Some(it.max) else None
  }

  implicit final class StringOps(private val s: String) extends AnyVal {

    /** The number the text is, if it is one. */
    def toIntOpt: Option[Int] =
      try Some(s.toInt)
      catch { case _: NumberFormatException => None }

    /** `true` or `false` in any case, if that is what the text is. */
    def toBooleanOpt: Option[Boolean] =
      if (s.equalsIgnoreCase("true")) Some(true) else if (s.equalsIgnoreCase("false")) Some(false) else None
  }
}
