package reachablelinkage.linkage

import scala.annotation.tailrec

/** Growing open-addressing set of longs. Any long, including 0, is a valid element. */
private[linkage] final class LongSet:
  private var mask = 15
  private var used = new Array[Boolean](mask + 1)
  private var keys = new Array[Long](mask + 1)
  private var size = 0

  private def slot(key: Long, mask: Int): Int =
    val h = key * 0x9e3779b97f4a7c15L
    ((h ^ (h >>> 32)).toInt) & mask

  /** True if `key` was not already in the set. */
  def add(key: Long): Boolean =
    if (size + 1) * 2 > mask + 1 then grow()
    @tailrec def probe(i: Int): Int = if used(i) && keys(i) != key then probe((i + 1) & mask) else i
    val i = probe(slot(key, mask))
    if used(i) then false
    else
      used(i) = true
      keys(i) = key
      size += 1
      true

  def isEmpty: Boolean = size == 0

  def contains(key: Long): Boolean =
    @tailrec def find(i: Int): Boolean = used(i) && (keys(i) == key || find((i + 1) & mask))
    find(slot(key, mask))

  def foreach(f: Long => Unit): Unit =
    @tailrec def from(i: Int): Unit =
      if i < keys.length then
        if used(i) then f(keys(i))
        from(i + 1)
    from(0)

  private def grow(): Unit =
    val (oldUsed, oldKeys) = (used, keys)
    mask = (mask + 1) * 2 - 1
    used = new Array[Boolean](mask + 1)
    keys = new Array[Long](mask + 1)
    @tailrec def free(j: Int): Int = if used(j) then free((j + 1) & mask) else j
    @tailrec def from(i: Int): Unit =
      if i < oldKeys.length then
        if oldUsed(i) then
          val j = free(slot(oldKeys(i), mask))
          used(j) = true
          keys(j) = oldKeys(i)
        from(i + 1)
    from(0)
