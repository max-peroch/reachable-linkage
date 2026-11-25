package reachablelinkage.linkage

/** Growable int list that also works as a stack; avoids boxing and captured-var cells. */
private[linkage] final class IntList:
  private var a = new Array[Int](16)
  private var n = 0

  def isEmpty: Boolean   = n == 0
  def size: Int          = n
  def apply(i: Int): Int = a(i)

  def push(x: Int): Unit =
    if n == a.length then a = java.util.Arrays.copyOf(a, n * 2)
    a(n) = x
    n += 1

  def pop(): Int =
    n -= 1
    a(n)

  def toArray: Array[Int] = java.util.Arrays.copyOf(a, n)
