package reachablelinkage.classfile

/** Why a class could not be turned into a [[ClassInfo]]. */
enum ClassFileError:
  /** The entry lists the class, but its bytes could not be read. */
  case Unavailable
  case Truncated
  case UnsupportedVersion(major: Int)
  /** ASM rejected the bytes; `detail` is its message. */
  case Malformed(detail: String)

  def message: String = this match
    case Unavailable            => "listed but unreadable"
    case Truncated              => "truncated class file"
    case UnsupportedVersion(v)  => s"unsupported class file version $v"
    case Malformed(detail)      => s"malformed class file: $detail"
