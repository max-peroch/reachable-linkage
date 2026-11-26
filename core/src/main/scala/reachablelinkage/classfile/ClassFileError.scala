package reachablelinkage.classfile

/** Why a class could not be turned into a `ClassInfo`. */
sealed abstract class ClassFileError extends Product with Serializable {
  import ClassFileError._
  def ordinal: Int
  def message: String = this match {
    case Unavailable           => "listed but unreadable"
    case Truncated             => "truncated class file"
    case UnsupportedVersion(v) => s"unsupported class file version $v"
    case Malformed(detail)     => s"malformed class file: $detail"
  }
}

object ClassFileError {

  /** The entry lists the class, but its bytes could not be read. */

  case object Unavailable                         extends ClassFileError { def ordinal: Int = 0 }
  case object Truncated                           extends ClassFileError { def ordinal: Int = 1 }
  final case class UnsupportedVersion(major: Int) extends ClassFileError { def ordinal: Int = 2 }

  /** ASM rejected the bytes; `detail` is its message. */

  final case class Malformed(detail: String) extends ClassFileError { def ordinal: Int = 3 }
}
