package reachablelinkage.symbols

import java.util.HashMap
import scala.collection.mutable.ArrayBuffer

/** Maps strings to dense int ids. Not thread-safe: one per analysis, confined to one thread. */
private[symbols] final class Interner {
  private val ids   = new HashMap[String, Integer]()
  private val names = ArrayBuffer.empty[String]

  def intern(s: String): Int = {
    val found = ids.get(s)
    if (found != null) found.intValue
    else {
      val id = names.length
      names += s
      ids.put(s, id)
      id
    }
  }

  def lookup(s: String): Int = {
    val found = ids.get(s)
    if (found == null) -1 else found.intValue
  }

  def string(id: Int): String = names(id)
  def size: Int               = names.length
}
