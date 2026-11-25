package reachablelinkage.symbols

/** Interned JVM descriptor, e.g. `(I)V`. */
opaque type DescId = Int
object DescId:
  inline def apply(i: Int): DescId            = i
  extension (d: DescId) inline def value: Int = d
