package reachablelinkage.linkage

/** One offending reference. `refIndex` is the entry's position in the class's `methodRefs` (per four ints), `fieldRefs` (per three) or `classRefs` array, and
  * -1 for [[RefKind.Hierarchy]].
  */
final case class Finding(conflict: Conflict, kind: RefKind, refIndex: Int, label: Label = Label.Reachable)
