package reachablelinkage.classfile

/** A fact the parser notes about a class that is not a call or a reference but something code does with a name or a type at run time. One case per kind of
  * fact, so a reader has to say what it does with each (the compiler checks the match), and nothing outside this file knows how a probe is stored.
  */
sealed abstract class Probe extends Product with Serializable {
  def ordinal: Int
}

object Probe {

  /** A string constant that has the shape of a class name (`Class.forName("a.B")`, `isAvailable("a.B")`): an internal name. */

  final case class ClassName(internalName: String) extends Probe { def ordinal: Int = 0 }

  /** The end of a class name a class that loads by name holds (`".kqueue.KQueue"` as `/kqueue/KQueue`): it names every class that ends with it. */

  final case class NameTail(suffix: String) extends Probe { def ordinal: Int = 1 }

  /** A constant method name passed to a reflective lookup (`getMethod("run")`). */

  final case class MethodName(name: String) extends Probe { def ordinal: Int = 2 }

  /** The functional interface a lambda or method reference makes an instance of (`invokedynamic`): an internal name. */

  final case class LambdaInterface(internalName: String) extends Probe { def ordinal: Int = 3 }

  // Stored as one string each (the summary file holds strings). A class name has no prefix and no class name starts with one of these.
  private val MethodPrefix: Char = '#'
  private val TailPrefix: Char   = '~'
  private val LambdaPrefix: Char = '^'

  /** No probes: shared, since most classes have none and nothing changes it. */
  val NoProbes: Array[Probe] = Array.empty[Probe]

  /** The stored form of a probe. */
  def encode(probe: Probe): String = probe match {
    case ClassName(n)       => n
    case NameTail(s)        => TailPrefix.toString + s
    case MethodName(n)      => MethodPrefix.toString + n
    case LambdaInterface(n) => LambdaPrefix.toString + n
  }

  /** The probe a stored string says. */
  def decode(stored: String): Probe =
    if (stored.isEmpty) ClassName(stored)
    else
      stored.charAt(0) match {
        case TailPrefix   => NameTail(stored.substring(1))
        case MethodPrefix => MethodName(stored.substring(1))
        case LambdaPrefix => LambdaInterface(stored.substring(1))
        case _            => ClassName(stored)
      }
}
