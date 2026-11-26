package reachablelinkage.classpath

import java.nio.file.Path

/** Why a classpath entry could not be opened. */
sealed abstract class SourceError extends Product with Serializable {
  import SourceError._
  def ordinal: Int
  def message: String = this match {
    case NotFound(p)      => s"not found: $p"
    case CannotOpen(p, c) => s"cannot open $p: $c"
  }
}

object SourceError {
  final case class NotFound(path: Path)                  extends SourceError { def ordinal: Int = 0 }
  final case class CannotOpen(path: Path, cause: String) extends SourceError { def ordinal: Int = 1 }
}
