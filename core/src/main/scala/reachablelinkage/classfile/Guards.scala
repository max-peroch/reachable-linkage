package reachablelinkage.classfile

/** The error types a `catch` can name that matter to linkage, as bits. The parser records, for each reference inside a protected range, which of these the
  * enclosing handlers catch.
  */
private[reachablelinkage] object Guards {
  val NoClassDef: Int              = 1
  val NoSuchMethod: Int            = 2
  val NoSuchField: Int             = 4
  val IncompatibleClassChange: Int = 8
  val IllegalAccess: Int           = 16
  val AbstractMethod: Int          = 32
  val Linkage: Int                 = 64
  val Error: Int                   = 128
  val Throwable: Int               = 256
  val Instantiation: Int           = 512

  /** A call on an exception that a handler makes to decide what to do with it (`e.getMessage()`). */
  def inspectsException(name: String): Boolean =
    name == "getMessage" || name == "getLocalizedMessage" || name == "getCause" || name == "getClass" || name == "toString"

  /** The bit for a caught exception type (internal name), or 0 if it cannot catch a linkage error. */
  def bitOf(caught: String): Int = caught match {
    case "java/lang/NoClassDefFoundError"         => NoClassDef
    case "java/lang/NoSuchMethodError"            => NoSuchMethod
    case "java/lang/NoSuchFieldError"             => NoSuchField
    case "java/lang/IncompatibleClassChangeError" => IncompatibleClassChange
    case "java/lang/IllegalAccessError"           => IllegalAccess
    case "java/lang/AbstractMethodError"          => AbstractMethod
    case "java/lang/InstantiationError"           => Instantiation
    case "java/lang/LinkageError"                 => Linkage
    case "java/lang/Error"                        => Error
    case "java/lang/Throwable"                    => Throwable
    case _                                        => 0
  }
}
