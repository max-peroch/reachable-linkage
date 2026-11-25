package reachablelinkage.linkage

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.symbols.Symbols

class ScopeSuite extends munit.FunSuite:

  private val index = ClassIndex(Vector(ClassSource.jdk()), new Symbols)

  test("a scope over entries the index has is built") {
    assert(Scope(index, Vector(0), Vector(0), Vector(0)).isRight)
  }

  test("each list that names a missing entry is reported by name, with the entry and the entry count") {
    def err(roots: Vector[Int] = Vector(0), checked: Vector[Int] = Vector(0), traverse: Vector[Int] = Vector(0), app: Vector[Int] = Vector(0)) =
      Scope(index, roots, checked, traverse, app).left.toOption
    assertEquals(err(roots = Vector(3)), Some(ScopeError.UnknownEntry(ScopeError.List.Roots, 3, 1)))
    assertEquals(err(checked = Vector(-1)), Some(ScopeError.UnknownEntry(ScopeError.List.Checked, -1, 1)))
    assertEquals(err(traverse = Vector(1)), Some(ScopeError.UnknownEntry(ScopeError.List.Traverse, 1, 1)))
    assertEquals(err(app = Vector(0, 2)), Some(ScopeError.UnknownEntry(ScopeError.List.App, 2, 1)))
  }

  test("the message names the list") {
    assertEquals(ScopeError.UnknownEntry(ScopeError.List.Roots, 3, 1).message, "roots names entry 3, but the index has 1 entries")
  }
