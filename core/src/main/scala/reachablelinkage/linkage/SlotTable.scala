package reachablelinkage.linkage

import scala.annotation.tailrec

/**
 * Growing open-addressing map from a method key to the graph node that stands for "a virtual call
 * to this method on this owner". Keys can be any long, including 0.
 */
private[linkage] final class SlotTable:
  private var mask   = 15
  private var used   = new Array[Boolean](mask + 1)
  private var keys   = new Array[Long](mask + 1)
  private var nodes  = new Array[Int](mask + 1)
  private var size   = 0

  private def slot(key: Long, mask: Int): Int =
    val h = key * 0x9e3779b97f4a7c15L
    ((h ^ (h >>> 32)).toInt) & mask

  /** The node for `key`, or -1 if none was put. */
  def get(key: Long): Int =
    @tailrec def find(i: Int): Int =
      if !used(i) then -1
      else if keys(i) == key then nodes(i)
      else find((i + 1) & mask)
    find(slot(key, mask))

  def contains(key: Long): Boolean = get(key) >= 0

  def put(key: Long, node: Int): Unit =
    if (size + 1) * 2 > mask + 1 then grow()
    @tailrec def probe(i: Int): Int = if used(i) && keys(i) != key then probe((i + 1) & mask) else i
    val i = probe(slot(key, mask))
    if !used(i) then size += 1
    used(i) = true
    keys(i) = key
    nodes(i) = node

  def foreach(f: (Long, Int) => Unit): Unit =
    @tailrec def from(i: Int): Unit =
      if i < keys.length then
        if used(i) then f(keys(i), nodes(i))
        from(i + 1)
    from(0)

  private def grow(): Unit =
    val (oldUsed, oldKeys, oldNodes) = (used, keys, nodes)
    mask = (mask + 1) * 2 - 1
    used = new Array[Boolean](mask + 1)
    keys = new Array[Long](mask + 1)
    nodes = new Array[Int](mask + 1)
    size = 0
    @tailrec def from(i: Int): Unit =
      if i < oldKeys.length then
        if oldUsed(i) then put(oldKeys(i), oldNodes(i))
        from(i + 1)
    from(0)
