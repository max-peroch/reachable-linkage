package reachablelinkage.api

import java.nio.file.Path

/** Why a class-load log could not be used. */
sealed abstract class ClassLoadLogError extends Product with Serializable {
  import ClassLoadLogError._
  def ordinal: Int
  def message: String = this match {
    case Unreadable(f, c)   => s"cannot read $f: $c"
    case NoClassesLogged(f) => s"$f has no class-load lines (run the application with -Xlog:class+load:file=<log>)"
  }
}

object ClassLoadLogError {
  final case class Unreadable(file: Path, cause: String) extends ClassLoadLogError { def ordinal: Int = 0 }

  /** The file has no class-load line at all, so it is not a log made with `-Xlog:class+load` (or `-verbose:class`). */

  final case class NoClassesLogged(file: Path) extends ClassLoadLogError { def ordinal: Int = 1 }
}
