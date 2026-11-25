package reachablelinkage.linkage

import scala.annotation.tailrec

/**
 * Open-addressing table from (owner, member key, kind) to a lookup result. Replaces a
 * `HashMap[(Int, Long, Int), _]` that allocated a tuple and boxed two values on every lookup.
 * Not thread-safe.
 */
private[linkage] final class ResolutionMemo:
  private var mask   = 1023
  private var size   = 0
  private var owners = new Array[Int](mask + 1)
  private var keys   = new Array[Long](mask + 1)
  private var kinds  = new Array[Int](mask + 1)
  private var values = new Array[Resolution | Null](mask + 1)

  private def hash(owner: Int, key: Long, kind: Int): Int =
    var h = (key * 0x9e3779b97f4a7c15L) ^ (owner.toLong * 0xc2b2ae3d27d4eb4fL) ^ kind.toLong
    h ^= h >>> 29
    (h ^ (h >>> 32)).toInt

  /** The stored result, or null if this lookup has not been done. */
  def get(owner: Int, key: Long, kind: Int): Resolution | Null =
    @tailrec def find(i: Int): Resolution | Null =
      if values(i) == null then null
      else if owners(i) == owner && keys(i) == key && kinds(i) == kind then values(i)
      else find((i + 1) & mask)
    find(hash(owner, key, kind) & mask)

  def put(owner: Int, key: Long, kind: Int, value: Resolution): Unit =
    if (size + 1) * 2 > mask + 1 then grow()
    insert(owner, key, kind, value)

  private def insert(owner: Int, key: Long, kind: Int, value: Resolution): Unit =
    @tailrec def probe(i: Int): Int =
      if values(i) != null && !(owners(i) == owner && keys(i) == key && kinds(i) == kind) then probe((i + 1) & mask) else i
    val i = probe(hash(owner, key, kind) & mask)
    if values(i) == null then size += 1
    owners(i) = owner; keys(i) = key; kinds(i) = kind; values(i) = value

  private def grow(): Unit =
    val (os, ks, kd, vs) = (owners, keys, kinds, values)
    mask = (mask + 1) * 2 - 1
    owners = new Array[Int](mask + 1); keys = new Array[Long](mask + 1)
    kinds = new Array[Int](mask + 1); values = new Array[Resolution | Null](mask + 1)
    size = 0
    @tailrec def from(i: Int): Unit =
      if i < vs.length then
        val v = vs(i)
        if v != null then insert(os(i), ks(i), kd(i), v)
        from(i + 1)
    from(0)
