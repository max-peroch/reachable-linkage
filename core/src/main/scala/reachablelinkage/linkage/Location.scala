package reachablelinkage.linkage

/** Method and source line of an offending reference. Empty and 0 when unknown. */
final case class Location(methodName: String, methodDescriptor: String, line: Int)

object Location:
  val Unknown: Location = Location("", "", 0)
