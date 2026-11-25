package reachablelinkage.classfile

/** The operand-stack tracking of class literals on its own, fed the effects the parser reports, so each rule is tested without bytecode. */
class LiteralStackSuite extends munit.FunSuite:

  private def recorded(stack: LiteralStack, pops: Int, pushes: Int, carry: Boolean = false): List[Int] =
    val out = List.newBuilder[Int]
    stack.call(pops, pushes, carry)(out += _)
    out.result()

  test("a literal is the top of the stack until something is pushed over it") {
    val s = new LiteralStack
    s.push(7); s.effect(0, 1); s.effect(0, 1) // two loads above it
    assertEquals(recorded(s, 3, 0), List(7), "a call popping the literal and the two values")
    assert(s.isEmpty)
  }

  test("a call that pops fewer values than are above the literal leaves it for the next call") {
    val s = new LiteralStack
    s.push(7); s.effect(0, 1)
    assertEquals(recorded(s, 1, 1), Nil, "a call with one argument takes the value, not the literal under it")
    assertEquals(recorded(s, 2, 0), List(7), "the result and the literal go to the next call")
  }

  test("a literal popped by something that is not a call is gone") {
    val s = new LiteralStack
    s.push(7); s.effect(1, 0)
    assert(s.isEmpty)
  }

  test("two literals are told apart by depth") {
    val s = new LiteralStack
    s.push(1); s.push(2)
    assertEquals(recorded(s, 1, 0), List(2))
    assertEquals(recorded(s, 1, 0), List(1))
  }

  test("dup keeps a literal on top where it is, makes it also the value under the copy, and puts what was under it one deeper") {
    val s = new LiteralStack
    s.push(1); s.push(2); s.dup()
    assertEquals(recorded(s, 1, 0), List(2), "the top one")
    assertEquals(recorded(s, 1, 0), List(2), "and the value the copy was made of")
    assertEquals(recorded(s, 1, 0), List(1), "the one that was under them")
  }

  test("aastore sends a literal that is the stored value into the array that stays on the stack") {
    val s = new LiteralStack
    s.effect(0, 1); s.dup(); s.effect(0, 1) // arr, arr, index
    s.push(5); s.arrayStore()               // arr, arr, index, lit -> arr
    assertEquals(recorded(s, 1, 0), List(5), "the call that takes the array takes the literal")
  }

  test("a varargs array: each literal is stored through a copy of the array, and all of them end up with the array") {
    val s = new LiteralStack
    s.effect(0, 1); s.effect(1, 1)                      // size, new array
    s.dup(); s.effect(0, 1); s.push(10); s.arrayStore() // arr[0] = A
    s.dup(); s.effect(0, 1); s.push(20); s.arrayStore() // arr[1] = B
    assertEquals(recorded(s, 1, 0).sorted, List(10, 20), "the call that takes the array takes both")
  }

  test("clear forgets everything, and a literal under values a clear never saw is not invented") {
    val s = new LiteralStack
    s.push(1); s.clear()
    assert(s.isEmpty)
    s.effect(0, 1)
    assertEquals(recorded(s, 1, 0), Nil)
  }

  test("values count one each, and an effect on an empty stack does nothing") {
    val s = new LiteralStack
    s.effect(2, 1); s.effect(0, 3); s.arrayStore(); s.dup()
    assert(s.isEmpty)
    assertEquals(recorded(s, 5, 0), Nil)
  }

  test("a call that carries gives the literals it popped to its result, and the call that takes the result is told of them") {
    val s = new LiteralStack
    s.effect(0, 1); s.push(7) // a receiver, the literal
    assertEquals(recorded(s, 2, 1, carry = true), List(7), "typeFactory.constructType(Foo.class)")
    assertEquals(recorded(s, 1, 0), List(7), "and then whatever takes the JavaType")
    assert(s.isEmpty)
  }

  test("without carry the literal stops at the call that pops it") {
    val s = new LiteralStack
    s.push(7)
    assertEquals(recorded(s, 1, 1), List(7))
    assertEquals(recorded(s, 1, 0), Nil)
  }

  test("a literal under the values a carrying call does not pop stays where it is, and the carried one is on top") {
    val s = new LiteralStack
    s.push(1); s.push(2)
    assertEquals(recorded(s, 1, 1, carry = true), List(2))
    assertEquals(recorded(s, 1, 0), List(2), "the result")
    assertEquals(recorded(s, 1, 0), List(1), "the literal that was under")
  }

  test("a literal carried twice is not repeated: a result that stands for it once is one value") {
    val s = new LiteralStack
    s.push(3); s.dup() // the same literal at depth 1 and 2
    assertEquals(recorded(s, 2, 1, carry = true), List(3, 3), "both copies were popped, and each is told of")
    assertEquals(recorded(s, 1, 0), List(3), "but the result carries it once")
  }
