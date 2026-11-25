package reachablelinkage.linkage

/** The kinds of name a keep rule holds. They are all strings underneath, and a rule that is given the wrong kind (a file where an annotation belongs) does
  * nothing and says nothing, so each kind is its own type. Whether a name is well formed is the facade's business (`NameForm`); these only say what it is.
  */
object RuleNames:

  /** An internal class name (`org/springframework/stereotype/Component`): an annotation type, or a supertype. */
  opaque type ClassName = String
  object ClassName:
    def apply(internalName: String): ClassName    = internalName
    extension (name: ClassName) def value: String = name

  /** A call, `owner/Name.method`: the owner as an internal class name, the method after the last dot. */
  opaque type CallName = String
  object CallName:
    def apply(call: String): CallName            = call
    extension (name: CallName) def value: String = name

  /** A file name, or the end of a path (`reference.conf`, `META-INF/spring.factories`). */
  opaque type FileName = String
  object FileName:
    def apply(file: String): FileName            = file
    extension (name: FileName) def value: String = name

  /** A suffix added to a class name to name another (`_$logger`). */
  opaque type NameSuffix = String
  object NameSuffix:
    def apply(suffix: String): NameSuffix          = suffix
    extension (name: NameSuffix) def value: String = name
