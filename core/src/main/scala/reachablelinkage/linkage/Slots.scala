package reachablelinkage.linkage

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/** A table indexed by class id: dense, grows on write, and reads past the end are empty. */
private[linkage] final class Slots[A >: Null <: AnyRef] {
  private val buf = ArrayBuffer.empty[A]

  def get(i: Int): A = if (i >= 0 && i < buf.length) buf(i) else null

  def put(i: Int, a: A): Unit = {
    @tailrec def grow(): Unit = if (buf.length <= i) { buf += null; grow() }
    grow()
    buf(i) = a
  }

  def getOrCreate(i: Int)(make: => A): A = {
    val existing = get(i)
    if (existing != null) existing
    else {
      val a = make
      put(i, a)
      a
    }
  }
}
