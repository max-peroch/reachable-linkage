package reachablelinkage.api

import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.control.Exception.catching

/** A file that could not be read, as the file and why. Each reader wraps it in its own error. */
private[api] final case class ReadFailure(file: Path, cause: String)

/** The one place a text file is read: the lines, or the reason there are none. */
private[api] object ReadLines:
  /**
   * Decodes leniently: a byte that is not valid UTF-8 (a path in the platform encoding in a JVM log, say) becomes
   * U+FFFD and the file is still read, instead of failing on a character nobody asked about.
   */
  def of(file: Path): Either[ReadFailure, List[String]] =
    catching(classOf[IOException]).either(new String(Files.readAllBytes(file), UTF_8).linesIterator.toList)
      .left.map(e => ReadFailure(file, String.valueOf(e.getMessage)))
