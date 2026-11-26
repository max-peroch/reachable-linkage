package reachablelinkage.classfile

import scala.annotation.tailrec

/** Fixed-capacity open-addressing map from long keys to non-negative int values, built once and then only read. Used to find a method's index from its (name,
  * descriptor) key without boxing.
  */
private[classfile] final class LongIntMap(expected: Int) {
  private val mask = Integer.highestOneBit(math.max(expected, 1) * 2 - 1) * 2 - 1
  private val keys = new Array[Long](mask + 1)
  // Stored as value + 1 so that 0 means an empty slot.
  private val vals = new Array[Int](mask + 1)

  private def slot(key: Long): Int = {
    val h = key * 0x9e3779b97f4a7c15L
    ((h ^ (h >>> 32)).toInt) & mask
  }

  def put(key: Long, value: Int): Unit = {
    @tailrec def free(i: Int): Int = if (vals(i) != 0 && keys(i) != key) free((i + 1) & mask) else i
    val i                          = free(slot(key))
    keys(i) = key
    vals(i) = value + 1
  }

  /** The value for `key`, or -1 if absent. */
  def get(key: Long): Int = {
    @tailrec def find(i: Int): Int =
      if (vals(i) == 0) -1
      else if (keys(i) == key) vals(i) - 1
      else find((i + 1) & mask)
    find(slot(key))
  }
}
