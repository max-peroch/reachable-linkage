package reachablelinkage.classfile

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import org.objectweb.asm.{Label, Opcodes}

/**
 * Which `catch` handlers really protect each reference in a method, worked out while ASM streams the
 * instructions past (see [[Guards]]). A `try` range protects what is inside it, but only if its handler can
 * stop the error:
 *
 *  - a handler that catches and rethrows (`catch (Throwable t) { throw t; }`, the one `try`-with-resources
 *    generates) guards nothing, because the error still escapes;
 *  - unless it looks at the exception (`getMessage`, `instanceof`, ...) to decide, as slf4j's `MDC` does: then
 *    it can swallow the one it expects, and it still guards;
 *  - a handler whose code runs straight to a `throw` (no jump, switch, return or nested range before it) always throws
 *    something, so the error escapes as another exception (`throw new IllegalStateException(msg, e)`): it guards nothing,
 *    whether or not it looks at the exception first.
 *
 * A handler's body comes after the `try` it belongs to, so the answer for a reference is only known once the
 * method has been read: [[protect]] remembers which blocks were open at the reference, and [[finish]] settles
 * every one. Nested handlers share local slots, so the exception local a handler stores is matched to the
 * innermost (latest) handler that stored it.
 *
 * Used by one parse at a time, for one method at a time ([[startMethod]]).
 */
private[classfile] final class HandlerTracker:

  /** A `try` range whose handler catches a linkage error type. `open` is true while inside the range. */
  private final class TryBlock(val start: Label, val end: Label, val handler: Label, val bit: Int):
    var open = false
    var rethrows = false
    var inspects = false
    var alwaysThrows = false
    /** The local the handler stores the exception in, and the order its handler was met (nested handlers share slots). */
    var slot = -1
    var seq  = 0

  /** A protected reference's position in the guard array, and which blocks were open at it (a bit per block; blocks past 63 count always). */
  private final class PendingGuard(val position: Int, val openBlocks: Long, val fixedBits: Int)

  private val blocks  = ArrayBuffer.empty[TryBlock]
  private val pending = ArrayBuffer.empty[PendingGuard]
  private var active  = 0
  private var open    = 0L
  private var fixed   = 0
  // The blocks whose handler started most recently, and whether the next instruction should store the exception.
  private var handlerBlocks = List.empty[TryBlock]
  private var handlerSeq    = 0
  private var awaitingStore = false
  // True from the start of a handler while its code is a straight line (no jump, switch, return or other label yet).
  private var straight      = false
  // The local loaded by the previous instruction, or -1.
  private var prevAload     = -1

  def startMethod(): Unit =
    blocks.clear(); pending.clear()
    active = 0; open = 0L; fixed = 0
    handlerBlocks = Nil; handlerSeq = 0; awaitingStore = false; straight = false; prevAload = -1

  /** The error-type bits of every handler around the instruction being read, before any rethrow is taken into account. */
  def protectedBy: Int = active

  /** A `try` range whose handler catches the error type `bit` (see [[Guards.bitOf]]). */
  def tryBlock(start: Label, end: Label, handler: Label, bit: Int): Unit =
    blocks += TryBlock(start, end, handler, bit)

  /** Records that the guard array entry at `position` is protected by the blocks open now. */
  def protect(position: Int): Unit = pending += PendingGuard(position, open, fixed)

  // ASM visits a block's labels after the block itself, so a range opens at its start label and
  // closes at its end label while the instructions in between stream past.
  def label(l: Label): Unit =
    if blocks.nonEmpty then
      // Walks the blocks once: opens and closes ranges at this label, gathers what is open now
      // and the blocks whose handler starts here.
      @tailrec def scan(i: Int, mask: Int, openNow: Long, fixedNow: Int, here: List[TryBlock]): Unit =
        if i < blocks.length then
          val block = blocks(i)
          if block.start eq l then block.open = true
          if block.end eq l then block.open = false
          val handlers = if block.handler eq l then block :: here else here
          if block.open then
            scan(i + 1, mask | block.bit, if i < 63 then openNow | (1L << i) else openNow, if i < 63 then fixedNow else fixedNow | block.bit, handlers)
          else scan(i + 1, mask, openNow, fixedNow, handlers)
        else
          active = mask; open = openNow; fixed = fixedNow
          // Only a handler's own start label begins a straight line; any other label (a jump target, a nested range) ends it.
          straight = here.nonEmpty
          if here.nonEmpty then
            handlerSeq += 1
            here.foreach(b => { b.seq = handlerSeq; b.slot = -1 })
            handlerBlocks = here; awaitingStore = true
      scan(0, 0, 0L, 0, Nil)

  /** A jump, a switch or a return: the handler's code is no longer a straight line. */
  def branch(): Unit =
    straight = false
    other()

  /** An instruction that is neither a variable access, a throw, a call on a value, nor a type test. */
  def other(): Unit =
    prevAload = -1
    awaitingStore = false

  /** A local variable access: the first store after a handler starts is the exception being saved. */
  def varInsn(op: Int, slot: Int): Unit =
    if awaitingStore && op == Opcodes.ASTORE then handlerBlocks.foreach(_.slot = slot)
    awaitingStore = false
    prevAload = if op == Opcodes.ALOAD then slot else -1

  /** `athrow`: if it throws the local a handler stored the exception in, that handler rethrows. */
  def athrow(): Unit =
    if prevAload >= 0 then handlerOf(prevAload)(_.rethrows = true)
    if straight then handlerBlocks.foreach(_.alwaysThrows = true)
    prevAload = -1; awaitingStore = false

  /** A call: `e.getMessage()` and the like, made on the local just loaded, mean the handler decides from the exception. */
  def methodInsn(name: String): Unit =
    if prevAload >= 0 && Guards.inspectsException(name) then handlerOf(prevAload)(_.inspects = true)
    other()

  /** A type instruction: `e instanceof X`, on the local just loaded, also means the handler decides from the exception. */
  def typeInsn(op: Int): Unit =
    if op == Opcodes.INSTANCEOF && prevAload >= 0 then handlerOf(prevAload)(_.inspects = true)
    other()

  /** Applies `mark` to the blocks of the innermost handler that stored the exception in local `slot`. */
  private def handlerOf(slot: Int)(mark: TryBlock => Unit): Unit =
    blocks.iterator.filter(_.slot == slot).map(_.seq).maxOption.foreach: n =>
      blocks.foreach(b => if b.slot == slot && b.seq == n then mark(b))

  /**
   * Settles every protected reference of the method: the bits that stay are those of blocks that were open and
   * whose handler does not rethrow (or does, but looks at the exception first) and does not always throw. `settle(position, bits)` is
   * called for each one, so the caller can write the answer back into its guard array.
   */
  def finish(settle: (Int, Int) => Unit): Unit =
    @tailrec def maskOf(g: PendingGuard, i: Int, mask: Int): Int =
      if i >= blocks.length || i >= 63 then mask
      else
        val block = blocks(i)
        maskOf(g, i + 1, if (g.openBlocks & (1L << i)) != 0 && (!block.rethrows || block.inspects) && !block.alwaysThrows then mask | block.bit else mask)
    @tailrec def patch(p: Int): Unit =
      if p < pending.length then
        val g = pending(p)
        settle(g.position, maskOf(g, 0, g.fixedBits))
        patch(p + 1)
    patch(0)
    pending.clear()
