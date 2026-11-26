package reachablelinkage.linkage

/** How likely a conflict is to actually break the application.
  *
  *   - `Reachable`: at Class precision every conflict that is not guarded; at Method precision, the method holding the broken reference is reachable from the
  *     application's code.
  *   - `Latent`: at Method precision, the class is on the classpath and referenced, but nothing the application can reach calls into that method.
  *   - `Guarded`: a handler catches the error the broken reference would throw (the error itself, or any superclass such as `LinkageError`), so the code
  *     expects it. The handler can be around the reference or, at Method precision, around every call that reaches its method.
  *   - `Probed`: the missing class is named by a string constant in code that is reached (for instance `Class.forName("x.Y")`, an `isAvailable("x.Y")` check,
  *     or a message about it), so the code probably checks for the class before using it. A heuristic: it never removes a conflict, and `FailOn.ANY` still
  *     counts it.
  *   - `Reflective`: reachable, but only because a reflection rule (a constant class or method name, a keep rule such as `@Inject`) or the class-load log made
  *     the code a root: no call path from the application's own code leads to it. It can fail for real, and rests on a rule or a log, not on a call the
  *     application makes.
  *   - `Deserialized`: reachable only through `\$deserializeLambda\$`, the method the JDK calls to rebuild a serializable lambda from bytes, which names every
  *     lambda body of its class. Real only for closures actually sent over the wire (Spark, remoting), so it is the weakest reachable label and the default
  *     `FailOn` does not fail on it.
  *
  * A `Latent` conflict stays `Latent` whatever else is true: unreached code breaks nothing. Then `Guarded`, then `Probed`, then `Deserialized`, then
  * `Reflective`, then `Reachable`.
  */
sealed abstract class Label extends Product with Serializable {
  def ordinal: Int
}

object Label {
  case object Reachable    extends Label { def ordinal: Int = 0 }
  case object Latent       extends Label { def ordinal: Int = 1 }
  case object Guarded      extends Label { def ordinal: Int = 2 }
  case object Probed       extends Label { def ordinal: Int = 3 }
  case object Reflective   extends Label { def ordinal: Int = 4 }
  case object Deserialized extends Label { def ordinal: Int = 5 }
  val values: Array[Label]             = Array(Reachable, Latent, Guarded, Probed, Reflective, Deserialized)
  def fromOrdinal(ordinal: Int): Label = values(ordinal)
}
