package reachablelinkage.classpath

import java.io.{ByteArrayOutputStream, InputStream}

/** Reading a whole stream, as `InputStream.readAllBytes` does on Java 9 and later. */
private[reachablelinkage] object Streams {

  /** Every byte of `in`. `sizeHint` is the length if it is known (a zip entry says), and a negative number if not. */
  def readAll(in: InputStream, sizeHint: Long): Array[Byte] =
    if (sizeHint >= 0 && sizeHint <= Int.MaxValue) readExactly(in, sizeHint.toInt) else readUntilEnd(in)

  private def readExactly(in: InputStream, size: Int): Array[Byte] = {
    val bytes = new Array[Byte](size)
    @scala.annotation.tailrec
    def fill(read: Int): Int =
      if (read == size) read
      else {
        val n = in.read(bytes, read, size - read)
        if (n < 0) read else fill(read + n)
      }
    val filled = fill(0)
    // A stream shorter than it said: keep what it had, as `readAllBytes` would.
    if (filled == size) bytes else java.util.Arrays.copyOf(bytes, filled)
  }

  private def readUntilEnd(in: InputStream): Array[Byte] = {
    val out    = new ByteArrayOutputStream()
    val buffer = new Array[Byte](8192)
    @scala.annotation.tailrec
    def copy(): Unit = {
      val n = in.read(buffer)
      if (n >= 0) {
        out.write(buffer, 0, n)
        copy()
      }
    }
    copy()
    out.toByteArray
  }
}
