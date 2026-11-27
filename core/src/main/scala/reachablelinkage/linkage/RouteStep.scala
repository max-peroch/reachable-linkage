package reachablelinkage.linkage

import reachablelinkage.symbols.ClassId

/** One method on the route from the application to a conflict: method number `method` of class `inClass`. */
final case class RouteStep(inClass: ClassId, method: Int)
