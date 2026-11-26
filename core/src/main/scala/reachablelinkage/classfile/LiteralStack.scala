package reachablelinkage.classfile

/** The class literals (`Foo.class`) that are still on the operand stack, and how deep, so that a call can tell which of them are its arguments:
  * `Props.create(Worker.class, a, b)` has the literal under two more values, and `register(A.class, B.class)` has them in a varargs array.
  *
  * Every instruction between the literal and the call reports how many values it pops and pushes; a literal that is popped by something that is not a call is
  * gone, one under the popped values only moves down. Whatever cannot be followed exactly (a `long` or `double` stack operation, `swap`, `dup_x1`, a jump or a
  * label) forgets everything, so a mistake can lose a literal and never invent one. Values count one each, whatever their size in slots.
  */
private[classfile] final class LiteralStack {
  private var classes = new Array[Int](4)
  private var depths  = new Array[Int](4) // 1 is the top of the stack
  private var n       = 0

  def isEmpty: Boolean = n == 0

  /** Forgets every literal: the code is no longer a straight line, or an instruction is not modelled. */
  def clear(): Unit = n = 0

  /** A literal of `cls` was just pushed: it is the top, and what it covered goes one deeper. */
  def push(cls: Int): Unit = {
    effect(0, 1)
    add(cls, 1)
  }

  /** An instruction that pops `pops` values and pushes `pushes`: a literal it pops is gone, one below only moves. */
  def effect(pops: Int, pushes: Int): Unit =
    if (n > 0) {
      var kept = 0
      var i    = 0
      while (i < n) {
        if (depths(i) > pops) {
          classes(kept) = classes(i); depths(kept) = depths(i) - pops + pushes; kept += 1
        }
        i += 1
      }
      n = kept
    }

  /** `dup`: the top value is copied. What was under it goes one deeper, and a literal that is the top is now also the value under the copy (`new Class[] {
    * A.class, B.class }` duplicates the array that carries the literals before each store, and the store pops the copy).
    */
  def dup(): Unit = {
    val before = n
    var i      = 0
    while (i < before) {
      if (depths(i) > 1) depths(i) += 1
      i += 1
    }
    i = 0
    while (i < before) {
      if (depths(i) == 1) add(classes(i), 2)
      i += 1
    }
  }

  /** `aastore` (pops the array, the index and the value): a literal that is the value now rides in the array that stays on the stack (`new Class[] { A.class
    * }`); the others lose the three values.
    */
  def arrayStore(): Unit = {
    var kept = 0
    var i    = 0
    while (i < n) {
      val d = depths(i)
      if (d == 1) { classes(kept) = classes(i); depths(kept) = 1; kept += 1 }
      else if (d > 3) { classes(kept) = classes(i); depths(kept) = d - 3; kept += 1 }
      i += 1
    }
    n = kept
  }

  /** A call that pops `pops` values (its arguments, and its receiver if it has one) and pushes `pushes`. `record` is told the class of each literal that was
    * one of the values it popped; those leave the stack, and the rest move down.
    *
    * With `carry`, the call's result stands for what it was given: `ClassTag.apply(Worker.class)` is a `ClassTag` of `Worker`,
    * `typeFactory.constructType(Dto.class)` a `JavaType` of `Dto`, `new Box(Foo.class)` a box of `Foo`. The literals it popped ride on the result, so the call
    * that takes the result is told of them too. This over-approximates (a result need not be about its argument), which a rule's choice of calls bounds.
    */
  def call(pops: Int, pushes: Int, carry: Boolean)(record: Int => Unit): Unit = {
    var carried = List.empty[Int]
    var i       = 0
    while (i < n) {
      if (depths(i) <= pops) {
        record(classes(i))
        if (carry && !carried.contains(classes(i))) carried = classes(i) :: carried
      }
      i += 1
    }
    effect(pops, pushes)
    carried.foreach(add(_, 1))
  }

  private def add(cls: Int, depth: Int): Unit = {
    if (n == classes.length) {
      classes = java.util.Arrays.copyOf(classes, n * 2)
      depths = java.util.Arrays.copyOf(depths, n * 2)
    }
    classes(n) = cls; depths(n) = depth; n += 1
  }
}
