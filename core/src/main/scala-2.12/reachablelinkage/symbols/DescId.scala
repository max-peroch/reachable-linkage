package reachablelinkage.symbols

/** An interned id: a value class, where Scala 3 has an opaque type over Int. */
final class DescId(val value: Int) extends AnyVal {
  override def toString: String = value.toString
}

object DescId {
  def apply(i: Int): DescId = new DescId(i)
}
