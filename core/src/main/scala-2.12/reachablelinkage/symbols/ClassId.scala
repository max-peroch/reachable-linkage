package reachablelinkage.symbols

/** An interned id: a value class, where Scala 3 has an opaque type over Int. */
final class ClassId(val value: Int) extends AnyVal {
  def isNone: Boolean           = value < 0
  override def toString: String = value.toString
}

object ClassId {
  def apply(i: Int): ClassId = new ClassId(i)
  val None: ClassId          = new ClassId(-1)
}
