package reachablelinkage.linkage

import scala.compiletime.constValueTuple
import scala.jdk.CollectionConverters.*
import scala.deriving.Mirror

import reachablelinkage.api.ConflictCategory
import reachablelinkage.classfile.{ClassFileError, Guards}
import reachablelinkage.symbols.Symbols

/**
 * `Conflict.describe` holds everything about a kind of conflict in one place (the class it is in, its facade category, which
 * handlers stop it, what it names, its texts). These tests keep it complete: a new case that is added without being described, or
 * a category the facade lacks, fails here and not in a rarely-run corner.
 */
class ConflictDescriptionSuite extends munit.FunSuite:

  private val syms = new Symbols
  private val a    = syms.classId("pkg/A")
  private val b    = syms.classId("pkg/B")
  private val n    = syms.nameId("m")
  private val d    = syms.descId("()V")

  /** How many cases a sum type has, from the compiler's own mirror. */
  private inline def labelCount[T](using m: Mirror.SumOf[T]): Int = constValueTuple[m.MirroredElemLabels].size

  /** One example of every case of `Conflict`. */
  private val examples: List[Conflict] = List(
    Conflict.MissingClass(a, b),
    Conflict.MissingMethod(a, n, d, b),
    Conflict.MethodKindChanged(a, n, d, b),
    Conflict.MissingField(a, n, d, b),
    Conflict.InaccessibleClass(a, b),
    Conflict.InaccessibleMethod(a, n, d, b),
    Conflict.InaccessibleField(a, n, d, b),
    Conflict.MissingProvider(a, b, 3),
    Conflict.AbstractMethodNotImplemented(a, b, n, d),
    Conflict.Unreadable(a, ClassFileError.Truncated),
    Conflict.IncompatibleHierarchy(a, b, HierarchyChange.SuperclassIsFinal),
    Conflict.OverridesFinalMethod(a, b, n, d),
    Conflict.InterfaceKindChanged(a, n, d, b, OwnerKind.Interface),
    Conflict.AbstractInstantiated(a, b, Uninstantiable.Abstract),
    Conflict.FinalFieldWritten(a, n, d, b),
    Conflict.MissingSignatureType(a, n, d, b),
    Conflict.NotExported(a, b)
  )

  test("the examples cover every case of Conflict, so a new case without an example fails here") {
    val cases = labelCount[Conflict]
    assertEquals(examples.map(_.ordinal).distinct.size, cases)
  }

  test("every conflict has a category the facade knows, and every facade category is produced by some conflict") {
    val produced = examples.map(_.describe.category.toString).toSet
    assert(produced.forall(name => ConflictCategory.values.exists(_.toString == name)), s"unknown to the facade: $produced")
    // The facade also has categories nothing here produces only if they are listed as such.
    assertEquals(ConflictCategory.values.map(_.toString).toSet -- produced, Set.empty[String])
    assertEquals(Category.values.map(_.toString).toSet, ConflictCategory.values.map(_.toString).toSet, "the two enums are in step")
  }

  test("the class that holds a conflict is the one described, and the texts name it") {
    examples.foreach: conflict =>
      val description = conflict.describe
      assertEquals(conflict.inClass, description.from)
      assert(conflict.show(syms).nonEmpty && description.reason(syms).nonEmpty, conflict.toString)
      assert(conflict.show(syms).contains(syms.className(description.from).replace('/', '.')), conflict.toString)
  }

  test("only conflicts with an instruction to protect can be guarded, and each by the error the JVM throws") {
    def stops(conflict: Conflict): Int = conflict.describe.stoppedBy
    val unguardable = examples.filter(stops(_) == 0).map(_.getClass.getSimpleName).toSet
    assertEquals(unguardable, Set("MissingProvider", "AbstractMethodNotImplemented", "Unreadable", "IncompatibleHierarchy", "OverridesFinalMethod", "MissingSignatureType"))
    assert((stops(examples.head) & Guards.NoClassDef) != 0, "a missing class is stopped by NoClassDefFoundError")
    assert((stops(Conflict.MissingMethod(a, n, d, b)) & Guards.NoSuchMethod) != 0)
    assert((stops(Conflict.MissingField(a, n, d, b)) & Guards.NoSuchField) != 0)
    assert((stops(Conflict.NotExported(a, b)) & Guards.IllegalAccess) != 0)
    assert((stops(Conflict.AbstractInstantiated(a, b, Uninstantiable.Abstract)) & Guards.Instantiation) != 0)
    // Every guardable one is also stopped by the broad errors (LinkageError, Error, Throwable) and by nothing as narrow as Exception.
    examples.filter(stops(_) != 0).foreach: conflict =>
      assert((stops(conflict) & (Guards.Linkage | Guards.Error | Guards.Throwable)) == (Guards.Linkage | Guards.Error | Guards.Throwable), conflict.toString)
  }

  test("a missing provider names the entry that ships the stale file, and only it") {
    assertEquals(Conflict.MissingProvider(a, b, 3).describe.declaredIn, Some(3))
    assertEquals(examples.filterNot(_.isInstanceOf[Conflict.MissingProvider]).flatMap(_.describe.declaredIn), Nil)
  }

  test("a member is named exactly when the conflict is about a member") {
    val withMember = examples.filter(_.describe.member.nonEmpty).map(_.getClass.getSimpleName).toSet
    assertEquals(withMember, Set("MissingMethod", "MethodKindChanged", "MissingField", "InaccessibleMethod", "InaccessibleField",
      "AbstractMethodNotImplemented", "OverridesFinalMethod", "InterfaceKindChanged", "FinalFieldWritten", "MissingSignatureType"))
  }

  test("the reasons the sbt plugin prints keep missinglink's wording, which differs from the one-line text") {
    assertEquals(Conflict.MissingClass(a, b).describe.reason(syms), "Class not found: pkg.A")
    assertEquals(Conflict.MissingMethod(a, n, d, b).describe.reason(syms), "Method not found: pkg.A.m()")
    assertEquals(Conflict.MissingField(a, n, d, b).describe.reason(syms), "Field not found: m")
    assertEquals(Conflict.MethodKindChanged(a, n, d, b).describe.reason(syms), "Method changed between static and instance: pkg.A.m()")
    assertEquals(Conflict.MissingClass(a, b).show(syms), "pkg.B: class pkg.A is missing")
  }


  test("the categories flagged as missinglink's are exactly the three missinglink-core defines") {
    val theirs = com.spotify.missinglink.Conflict.ConflictCategory.values.map(_.name).toSet
    val ours   = ConflictCategory.values.filter(_.reportedByMissinglink).map(_.toString).toSet
    assertEquals(ours, theirs)
    assert(theirs.forall(name => ConflictCategory.values.exists(_.toString == name)), "and they exist here under the same names")
  }

  test("missinglinkReports keeps only those categories, in order, whatever their labels") {
    import reachablelinkage.api.{ConflictLabel, Dependency, LinkageConflict, LinkageResult}
    def conflict(category: ConflictCategory, label: ConflictLabel) =
      LinkageConflict(category, Dependency("a.A", "m", "()V", 1, "b.B", "", ""), "entry", "entry", "reason", label)
    val all = List(
      conflict(ConflictCategory.CLASS_NOT_FOUND, ConflictLabel.LATENT),
      conflict(ConflictCategory.ABSTRACT_METHOD_NOT_IMPLEMENTED, ConflictLabel.REACHABLE),
      conflict(ConflictCategory.FIELD_NOT_FOUND, ConflictLabel.REACHABLE),
      conflict(ConflictCategory.NOT_EXPORTED, ConflictLabel.REACHABLE),
      conflict(ConflictCategory.METHOD_SIGNATURE_NOT_FOUND, ConflictLabel.GUARDED))
    val result = LinkageResult(java.util.List.copyOf(all.asJava), java.util.List.of(), java.util.List.of())
    assertEquals(result.missinglinkReports().asScala.map(_.category).toList,
      List(ConflictCategory.CLASS_NOT_FOUND, ConflictCategory.FIELD_NOT_FOUND, ConflictCategory.METHOD_SIGNATURE_NOT_FOUND))
  }

