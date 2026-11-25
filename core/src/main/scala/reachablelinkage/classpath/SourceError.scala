package reachablelinkage.classpath

import java.nio.file.Path

/** Why a classpath entry could not be opened. */
enum SourceError:
  case NotFound(path: Path)
  case CannotOpen(path: Path, cause: String)

  def message: String = this match
    case NotFound(p)      => s"not found: $p"
    case CannotOpen(p, c) => s"cannot open $p: $c"
