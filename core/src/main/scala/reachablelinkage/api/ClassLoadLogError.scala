package reachablelinkage.api

import java.nio.file.Path

/** Why a class-load log could not be used. */
enum ClassLoadLogError:
  case Unreadable(file: Path, cause: String)

  /** The file has no class-load line at all, so it is not a log made with `-Xlog:class+load` (or `-verbose:class`). */
  case NoClassesLogged(file: Path)

  def message: String = this match
    case Unreadable(f, c)   => s"cannot read $f: $c"
    case NoClassesLogged(f) => s"$f has no class-load lines (run the application with -Xlog:class+load:file=<log>)"
