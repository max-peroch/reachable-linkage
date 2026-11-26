package reachablelinkage.api

import java.nio.file.Path

/** A jar or class directory, with the name conflicts are attributed to (typically module id). */
final case class ClasspathEntry(name: String, path: Path)
