package reachablelinkage.classfile

/** Calls that reach code by a name given as a string, which the parser notes so the call graph can follow them. */
private[reachablelinkage] object ReflectionCalls {

  /** A call that looks a method up by the name just loaded: `Class.getMethod`, a `MethodHandles.Lookup` find. */
  def isMemberLookup(owner: String, name: String): Boolean =
    (owner == "java/lang/Class" && (name == "getMethod" || name == "getDeclaredMethod")) ||
      (owner == "java/lang/invoke/MethodHandles$Lookup" && (name == "findVirtual" || name == "findStatic" || name == "findSpecial"))

  /** A call that loads a class by the name it is given: `Class.forName`, `ClassLoader.loadClass`, a `Lookup.findClass`. */
  def isClassLoad(owner: String, name: String): Boolean =
    (owner == "java/lang/Class" && name == "forName") ||
      (owner == "java/lang/ClassLoader" && name == "loadClass") ||
      (owner == "java/lang/invoke/MethodHandles$Lookup" && name == "findClass")
}
