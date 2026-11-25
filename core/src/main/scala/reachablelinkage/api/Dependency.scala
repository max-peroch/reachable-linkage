package reachablelinkage.api

/** The reference behind a conflict. Class names are dotted, as in missinglink-core. `fromLineNumber` is 0 and `fromMethodName` is empty when the reference is
  * not an instruction (a missing superclass).
  */
final case class Dependency(
    fromClass: String,
    fromMethodName: String,
    fromMethodDescriptor: String,
    fromLineNumber: Int,
    targetClass: String,
    targetMemberName: String,
    targetMemberDescriptor: String
):
  def describe(): String =
    val member = if targetMemberName.isEmpty then "" else s".$targetMemberName"
    s"$fromClass${if fromMethodName.isEmpty then "" else s"#$fromMethodName"} -> $targetClass$member"
