package reachablelinkage.linkage

/** How finely reachability is tracked. */
enum Precision:
  /** A class is reachable if anything reachable names it; all its references count. */
  case Class
  /** A class's reference counts only if the method holding it is reachable in the call graph (class hierarchy analysis). */
  case Method
  /** As `Method`, but a call can only land in a class that is instantiated (see [[Dispatch.Rta]]). */
  case MethodRta
