package reachablelinkage.classfile

import scala.annotation.tailrec

/** Recognises string constants that look like class names, such as `"android.util.Base64"` or `"org/slf4j/impl/StaticLoggerBinder"`. Code that checks whether
  * an optional class is present names it this way (`Class.forName`, `isAvailable`, an error message). Kept narrow so ordinary strings and configuration keys do
  * not match: dotted or slashed identifiers of at least two segments, no spaces, and a last segment that starts with an upper-case letter.
  */
private[reachablelinkage] object ProbeNames:

  /** The internal name (`a/b/C`) if `s` looks like a class name, else null. */
  def internalName(s: String): String | Null =
    val n                                                                                            = s.length
    @tailrec def scan(i: Int, separators: Int, segmentStart: Boolean, lastStart: Int): String | Null =
      if i == n then if !segmentStart && separators >= 1 && Character.isUpperCase(s.charAt(lastStart)) then s.replace('.', '/') else null
      else
        val c = s.charAt(i)
        if c == '.' || c == '/' then if segmentStart then null else scan(i + 1, separators + 1, true, lastStart)
        else if segmentStart then if Character.isJavaIdentifierStart(c) then scan(i + 1, separators, false, i) else null
        else if Character.isJavaIdentifierPart(c) then scan(i + 1, separators, false, lastStart)
        else null
    if n < 3 || n > 200 then null else scan(0, 0, true, 0)

  /** The suffix (`/kqueue/KQueue`) of a fragment such as `".kqueue.KQueue"`: a name that starts with a dot and has at least two segments, the last one starting
    * with an upper-case letter. Code that builds a class name from a prefix it computes (`pkg + ".kqueue.KQueue"`) holds the rest as a constant like this; null
    * if `s` is not one.
    */
  def fragment(s: String): String | Null =
    if s.length < 5 || s.length > 120 || s.charAt(0) != '.' then null
    else
      val body = internalName(s.substring(1))
      if body == null then null else "/" + body.nn
