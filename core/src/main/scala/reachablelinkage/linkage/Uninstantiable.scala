package reachablelinkage.linkage

/** Why a class can no longer be built with `new`. */
enum Uninstantiable:
  /** It is now an `abstract` class. */
  case Abstract
  /** It is now an interface. */
  case Interface
