package reachablelinkage.linkage

import reachablelinkage.classpath.ClassIndex

/** Which classpath entries (by index in the [[ClassIndex]]) take part in a check. Build one with [[Scope.apply]], which rejects indices that name no entry.
  *
  * @param roots
  *   entries whose classes start the class-level reachability walk
  * @param checked
  *   entries whose reachable classes have their references verified
  * @param traverse
  *   entries the reachability walk may enter; keep the JDK out of it for speed
  * @param app
  *   the application's own entries: every method of their classes starts the call graph at Method precision. Usually the same as `roots`, but a whole-classpath
  *   scan has every class as a root and still measures reachability from the application.
  */
final class Scope private (
    val roots: IndexedSeq[Int],
    val checked: IndexedSeq[Int],
    val traverse: IndexedSeq[Int],
    val app: IndexedSeq[Int]
)

object Scope:
  /** The scope of a run over the entries it opened, which cannot go wrong: an entry outside the index is left out, so there is no error to return.
    * `Scope.apply` is for callers whose indices come from elsewhere and must be told when one is bad.
    */
  def over(index: ClassIndex, roots: IndexedSeq[Int], checked: IndexedSeq[Int], traverse: IndexedSeq[Int], app: IndexedSeq[Int]): Scope =
    val n                                                  = index.sources.length
    def inIndex(entries: IndexedSeq[Int]): IndexedSeq[Int] = entries.filter(i => i >= 0 && i < n)
    new Scope(inIndex(roots), inIndex(checked), inIndex(traverse), inIndex(app))

  /** A scope whose application entries are its roots. */
  def apply(
      index: ClassIndex,
      roots: IndexedSeq[Int],
      checked: IndexedSeq[Int],
      traverse: IndexedSeq[Int]
  ): Either[ScopeError, Scope] = apply(index, roots, checked, traverse, roots)

  def apply(
      index: ClassIndex,
      roots: IndexedSeq[Int],
      checked: IndexedSeq[Int],
      traverse: IndexedSeq[Int],
      app: IndexedSeq[Int]
  ): Either[ScopeError, Scope] =
    val n                                                                                           = index.sources.length
    def known(list: ScopeError.List, entries: IndexedSeq[Int]): Either[ScopeError, IndexedSeq[Int]] =
      entries.find(i => i < 0 || i >= n).fold(Right(entries))(i => Left(ScopeError.UnknownEntry(list, i, n)))
    for
      r <- known(ScopeError.List.Roots, roots)
      c <- known(ScopeError.List.Checked, checked)
      t <- known(ScopeError.List.Traverse, traverse)
      a <- known(ScopeError.List.App, app)
    yield new Scope(r, c, t, a)
