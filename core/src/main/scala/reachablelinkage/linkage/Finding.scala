package reachablelinkage.linkage

/** One offending reference. `refIndex` is the entry's position in the class's `methodRefs` (per four ints), `fieldRefs` (per three) or `classRefs` array, and
  * -1 for [[RefKind.Hierarchy]].
  */
final case class Finding(
    conflict: Conflict,
    kind: RefKind,
    refIndex: Int,
    label: Label = Label.Reachable,
    /** The methods that lead from the application to the one holding the reference, first to last: filled for the conflicts that fail, when routes were asked
      * for.
      */
    route: Vector[RouteStep] = Vector.empty
)
