package reachablelinkage.classfile

import org.objectweb.asm.{Label, Opcodes}

/**
 * The guard analysis on its own, fed the events ASM would send, so each rule is tested without compiling
 * Java: the cases here include ones `javac` does not emit.
 */
class HandlerTrackerSuite extends munit.FunSuite:

  private val NoSuchMethod = Guards.NoSuchMethod
  private val NoClassDef   = Guards.NoClassDef

  /** One method read through a tracker: the guard array it fills, and the label events around the `try`. */
  private final class Method(bit: Int = NoSuchMethod):
    val tracker = new HandlerTracker
    val guards  = new IntBuf
    val start, end, handler, after = new Label
    tracker.startMethod()
    tracker.tryBlock(start, end, handler, bit)

    /** A reference recorded now, protected by whatever is open: returns where its mask sits in `guards`. */
    def reference(): Int =
      val bits = tracker.protectedBy
      if bits == 0 then -1
      else
        guards.add(0); guards.add(0); guards.add(bits)
        tracker.protect(guards.length - 1)
        guards.length - 1

    def settle(): Array[Int] =
      tracker.finish(guards.set)
      guards.toArray

  private def maskAt(m: Method, position: Int): Int = m.settle()(position)

  test("a reference inside the range is protected by the handler's error type, one outside is not") {
    val m = new Method()
    assertEquals(m.reference(), -1, "before the range")
    m.tracker.label(m.start)
    val inside = m.reference()
    m.tracker.label(m.end)
    assertEquals(m.reference(), -1, "after the range")
    assertEquals(maskAt(m, inside), NoSuchMethod)
  }

  test("a handler that swallows the error keeps its guard") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1); m.tracker.other()   // astore e; then some code
    assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  test("a handler that loads the exception it stored and throws it guards nothing") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.athrow()
    assertEquals(maskAt(m, ref), 0)
  }

  test("throwing some other local is not a rethrow of the exception") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1); m.tracker.branch()   // a branch first: not a straight line to the throw
    m.tracker.varInsn(Opcodes.ALOAD, 2); m.tracker.athrow()   // a different exception object
    assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  test("a throw that is not of a loaded local (a new exception) is not a rethrow") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1); m.tracker.branch()
    m.tracker.methodInsn("<init>"); m.tracker.athrow()
    assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  // ---- a handler that always throws something ----

  test("a handler whose code runs straight to a throw of a new exception guards nothing: the error escapes wrapped") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.other(); m.tracker.other()               // new X; dup
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.methodInsn("<init>"); m.tracker.athrow()   // X(e); athrow
    assertEquals(maskAt(m, ref), 0)
  }

  test("reading the message first does not make a handler that always throws a guard") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.methodInsn("getMessage")   // inspects, then
    m.tracker.other(); m.tracker.methodInsn("<init>"); m.tracker.athrow()    // throws a new exception whatever it said
    assertEquals(maskAt(m, ref), 0)
  }

  test("a branch before the throw keeps the guard: the handler may swallow the error on another path") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.branch()                                 // if (...) return;
    m.tracker.other(); m.tracker.methodInsn("<init>"); m.tracker.athrow()
    assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  test("a return before the throw, a switch, or any label in the handler (a jump target, a nested range) keeps the guard too") {
    for breaks <- List[HandlerTracker => Unit](_.branch(), _.label(new Label)) do
      val m = new Method()
      m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
      m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
      breaks(m.tracker)
      m.tracker.other(); m.tracker.methodInsn("<init>"); m.tracker.athrow()
      assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  test("a handler that only logs and falls out of the block still guards (nothing throws)") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1); m.tracker.methodInsn("println"); m.tracker.branch()   // goto after
    m.tracker.label(m.after)
    assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  test("a throw straight after a handler that is not the one the reference was in does not affect it") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1); m.tracker.branch()
    m.tracker.label(m.after); m.tracker.other(); m.tracker.athrow()   // a later throw, no longer in the handler's straight line
    assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  test("a handler that reads the message before rethrowing can swallow the error it expects, so it guards") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.methodInsn("getMessage")
    m.tracker.branch()                                    // if (msg.contains("expected")) { ...; return; } as in slf4j's MDC
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.athrow()
    assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  test("reading the message and then rethrowing with nothing in between decides nothing: it always throws, so it guards nothing") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.methodInsn("getMessage")
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.athrow()
    assertEquals(maskAt(m, ref), 0)
  }

  test("a handler that tests the type of the exception also guards") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.typeInsn(Opcodes.INSTANCEOF)
    m.tracker.branch()                                    // the `if` on the result
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.athrow()
    assertEquals(maskAt(m, ref), NoSuchMethod)
  }

  test("a call on the exception that is not an inspection (addSuppressed) does not count as one") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.methodInsn("addSuppressed")
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.athrow()
    assertEquals(maskAt(m, ref), 0)
  }

  test("inspecting some other value is not inspecting the exception") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.varInsn(Opcodes.ALOAD, 2); m.tracker.methodInsn("getMessage")   // another object's message
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.athrow()
    assertEquals(maskAt(m, ref), 0)
  }

  test("the instruction between the load and the call must not intervene: a load that is not directly before does not count") {
    val m = new Method()
    m.tracker.label(m.start); val ref = m.reference(); m.tracker.label(m.end)
    m.tracker.label(m.handler); m.tracker.varInsn(Opcodes.ASTORE, 1)
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.other(); m.tracker.methodInsn("getMessage")
    m.tracker.varInsn(Opcodes.ALOAD, 1); m.tracker.athrow()
    assertEquals(maskAt(m, ref), 0)
  }

  test("nested handlers that reuse a local are told apart by order: the later one decides for its own block") {
    // Two blocks, both storing the exception in local 1 (slots are reused once a scope ends). Only the second rethrows.
    val tracker = new HandlerTracker; val guards = new IntBuf
    val s1, e1, h1, s2, e2, h2 = new Label
    tracker.startMethod()
    tracker.tryBlock(s1, e1, h1, NoSuchMethod)
    tracker.tryBlock(s2, e2, h2, NoClassDef)
    tracker.label(s1); guards.add(0); guards.add(0); guards.add(tracker.protectedBy); tracker.protect(2); tracker.label(e1)
    tracker.label(s2); guards.add(0); guards.add(0); guards.add(tracker.protectedBy); tracker.protect(5); tracker.label(e2)
    tracker.label(h1); tracker.varInsn(Opcodes.ASTORE, 1)                                    // swallows
    tracker.label(h2); tracker.varInsn(Opcodes.ASTORE, 1); tracker.varInsn(Opcodes.ALOAD, 1); tracker.athrow()   // rethrows
    tracker.finish(guards.set)
    assertEquals(guards.toArray.toList, List(0, 0, NoSuchMethod, 0, 0, 0))
  }

  test("a reference inside two ranges is guarded if either handler can stop the error") {
    val tracker = new HandlerTracker; val guards = new IntBuf
    val s1, e1, h1, s2, e2, h2 = new Label
    tracker.startMethod()
    tracker.tryBlock(s1, e1, h1, NoSuchMethod)   // outer, rethrows
    tracker.tryBlock(s2, e2, h2, Guards.Error)   // inner, swallows
    tracker.label(s1); tracker.label(s2)
    guards.add(0); guards.add(0); guards.add(tracker.protectedBy); tracker.protect(2)
    tracker.label(e2); tracker.label(e1)
    tracker.label(h2); tracker.varInsn(Opcodes.ASTORE, 2)
    tracker.label(h1); tracker.varInsn(Opcodes.ASTORE, 1); tracker.varInsn(Opcodes.ALOAD, 1); tracker.athrow()
    tracker.finish(guards.set)
    assertEquals(guards.toArray(2), Guards.Error, "only the swallowing handler's type is left")
  }

  test("a method's state does not leak into the next one") {
    val m = new Method()
    m.tracker.label(m.start)
    assertEquals(m.tracker.protectedBy, NoSuchMethod)
    m.tracker.startMethod()
    assertEquals(m.tracker.protectedBy, 0)
    val l = new Label; m.tracker.label(l)
    assertEquals(m.tracker.protectedBy, 0)
  }

  test("past 63 blocks the extra ones keep their bit for good: it is safe to over-approximate a guard, never to drop one") {
    val tracker = new HandlerTracker; val guards = new IntBuf
    val labels = (0 until 66).map(_ => (new Label, new Label, new Label))
    tracker.startMethod()
    labels.foreach((s, e, h) => tracker.tryBlock(s, e, h, NoSuchMethod))
    labels.foreach((s, _, _) => tracker.label(s))                       // every range open
    guards.add(0); guards.add(0); guards.add(tracker.protectedBy); tracker.protect(2)
    // Every handler stores local 1 and rethrows it, so none guards: but the blocks past 63 are not tracked.
    labels.foreach: (_, _, h) =>
      tracker.label(h); tracker.varInsn(Opcodes.ASTORE, 1); tracker.varInsn(Opcodes.ALOAD, 1); tracker.athrow()
    tracker.finish(guards.set)
    assertEquals(guards.toArray(2), NoSuchMethod, "the untracked blocks keep their bit")
  }
