package reachablelinkage.api

/** What a name the user types must look like. */
private[api] object JavaNames:
  /** A dotted name (`com.acme.Thing`, `com.acme`), each part a Java identifier; `$` is allowed, as in `Outer$Inner`. */
  def isQualified(name: String): Boolean =
    name.nonEmpty && name.split("\\.", -1).forall: part =>
      part.nonEmpty && Character.isJavaIdentifierStart(part.head) && part.tail.forall(Character.isJavaIdentifierPart)
