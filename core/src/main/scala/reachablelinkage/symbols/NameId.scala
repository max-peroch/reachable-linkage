package reachablelinkage.symbols

/** Interned simple name: method or field name. */
opaque type NameId = Int
object NameId:
  inline def apply(i: Int): NameId            = i
  extension (n: NameId) inline def value: Int = n
