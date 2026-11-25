package reachablelinkage.linkage

import java.util.BitSet

/** How each class and method of the call graph was first reached. The walk runs in phases, one per [[Reach]] in order of strength: the direct roots first, then
  * what only a rule makes a root, then what only deserialization does. Everything the walk first reaches during a phase is recorded for that phase, so a thing
  * reached by several kinds of root has the strongest, whichever order they were found in (the walk only adds, so a later phase never takes anything from an
  * earlier one).
  */
private[linkage] final class Provenance:
  // `Reach.values` copies its array on every call, so it is read once.
  private val kinds          = Reach.values
  private val classes        = Array.fill(kinds.length)(new BitSet)
  private val methods        = Array.fill(kinds.length)(new Slots[BitSet])
  private var current: Reach = Reach.Direct

  /** The phase the walk is in. */
  def phase: Reach = current

  /** The walk moves on to the phase of `reach`. */
  def enter(reach: Reach): Unit = current = reach

  /** A class was reached for the first time. */
  def reachedClass(c: Int): Unit = if phase != Reach.Direct then classes(phase.ordinal).set(c)

  /** A method of a class was reached for the first time. */
  def reachedMethod(c: Int, method: Int): Unit =
    if phase != Reach.Direct then methods(phase.ordinal).getOrCreate(c)(new BitSet).set(method)

  /** How the method (or, for -1, the class) was reached. */
  def of(c: Int, method: Int): Reach =
    def in(r: Reach): Boolean =
      if method < 0 then c >= 0 && classes(r.ordinal).get(c)
      else
        val bits = methods(r.ordinal).get(c)
        bits != null && bits.get(method)
    kinds.find(r => r != Reach.Direct && in(r)).getOrElse(Reach.Direct)
