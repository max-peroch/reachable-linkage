package reachablelinkage.symbols

/** An interned id: a value class, where Scala 3 has an opaque type over Int. */
final class NameId(val value: Int) extends AnyVal {
  override def toString: String = value.toString
}

object NameId {
  def apply(i: Int): NameId = new NameId(i)
}
