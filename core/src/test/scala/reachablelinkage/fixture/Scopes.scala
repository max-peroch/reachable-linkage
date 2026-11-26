package reachablelinkage.fixture

import reachablelinkage.classpath.ClassIndex
import reachablelinkage.linkage.Scope

/** Builds a [[Scope]] in tests; an invalid index there is a bug in the test, so it fails loudly. */
object Scopes:
  def of(index: ClassIndex, roots: IndexedSeq[Int], checked: IndexedSeq[Int], traverse: IndexedSeq[Int]): Scope =
    Scope(index, roots, checked, traverse).fold(e => throw new AssertionError(e.message), identity)
