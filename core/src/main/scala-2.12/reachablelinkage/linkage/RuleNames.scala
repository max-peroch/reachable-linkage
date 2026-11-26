package reachablelinkage.linkage

/** The kinds of name a keep rule holds. They are all strings underneath, and each kind is its own type (a value class here, an opaque type in Scala 3). */
object RuleNames {

  /** An internal class name (`org/springframework/stereotype/Component`): an annotation type, or a supertype. */
  final class ClassName(val value: String) extends AnyVal {
    override def toString: String = value
  }
  object ClassName {
    def apply(internalName: String): ClassName = new ClassName(internalName)
  }

  /** A call, `owner/Name.method`: the owner as an internal class name, the method after the last dot. */
  final class CallName(val value: String) extends AnyVal {
    override def toString: String = value
  }
  object CallName {
    def apply(call: String): CallName = new CallName(call)
  }

  /** A file name, or the end of a path (`reference.conf`, `META-INF/spring.factories`). */
  final class FileName(val value: String) extends AnyVal {
    override def toString: String = value
  }
  object FileName {
    def apply(file: String): FileName = new FileName(file)
  }

  /** A suffix added to a class name to name another (`_\$logger`). */
  final class NameSuffix(val value: String) extends AnyVal {
    override def toString: String = value
  }
  object NameSuffix {
    def apply(suffix: String): NameSuffix = new NameSuffix(suffix)
  }
}
