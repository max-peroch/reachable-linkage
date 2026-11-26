package reachablelinkage.classfile

/** Minimal growable int array, to avoid boxing in the visitor hot path. */
private final class IntBuf {
  private var a         = new Array[Int](12)
  private var n         = 0
  def add(x: Int): Unit = {
    if (n == a.length) a = java.util.Arrays.copyOf(a, n * 2)
    a(n) = x
    n += 1
  }
  def length: Int               = n
  def apply(i: Int): Int        = a(i)
  def set(i: Int, x: Int): Unit = a(i) = x
  def toArray: Array[Int]       = java.util.Arrays.copyOf(a, n)
}
