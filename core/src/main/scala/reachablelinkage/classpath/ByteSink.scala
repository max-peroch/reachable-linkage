package reachablelinkage.classpath

/** A growable big-endian byte buffer, for writing a summary file in one pass with patchable offsets. */
private[classpath] final class ByteSink:
  private var a = new Array[Byte](1 << 16)
  private var n = 0

  def size: Int = n

  private def ensure(extra: Int): Unit =
    if n + extra > a.length then a = java.util.Arrays.copyOf(a, math.max(a.length * 2, n + extra))

  def putInt(x: Int): Unit =
    ensure(4)
    a(n) = (x >>> 24).toByte; a(n + 1) = (x >>> 16).toByte; a(n + 2) = (x >>> 8).toByte; a(n + 3) = x.toByte
    n += 4

  def putShort(x: Int): Unit =
    ensure(2)
    a(n) = (x >>> 8).toByte; a(n + 1) = x.toByte
    n += 2

  def putBytes(b: Array[Byte]): Unit =
    ensure(b.length)
    System.arraycopy(b, 0, a, n, b.length)
    n += b.length

  /** Overwrites an int already written at `at`; used to fill in section offsets once they are known. */
  def setInt(at: Int, x: Int): Unit =
    a(at) = (x >>> 24).toByte; a(at + 1) = (x >>> 16).toByte; a(at + 2) = (x >>> 8).toByte; a(at + 3) = x.toByte

  def toArray: Array[Byte] = java.util.Arrays.copyOf(a, n)
