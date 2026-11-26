package reachablelinkage.api

/** How many conflicts one package filter removed, so a build can say "N conflicts found in ignored source packages". */
final case class FilterEffect(kind: FilterKind, removed: Int)
