package reachablelinkage.linkage

/** How a class's declared superclass or interface no longer fits (the JVM throws `IncompatibleClassChangeError` loading the class). */
enum HierarchyChange:
  /** The superclass became `final`. */
  case SuperclassIsFinal
  /** The superclass became an interface. */
  case SuperclassIsInterface
  /** A type the class implements became a class. */
  case InterfaceIsClass
