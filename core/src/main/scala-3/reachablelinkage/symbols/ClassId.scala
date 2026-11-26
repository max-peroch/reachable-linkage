package reachablelinkage.symbols

/** Interned internal class name, e.g. `java/lang/Object`. */
opaque type ClassId = Int
object ClassId {
  @inline def apply(i: Int): ClassId = i
  val None: ClassId                  = -1
  extension (c: ClassId) {
    @inline def value: Int      = c
    @inline def isNone: Boolean = c < 0
  }
}
