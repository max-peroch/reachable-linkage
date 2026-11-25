package reachablelinkage.linkage

import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.JavaFixture.{compile, source}
import reachablelinkage.symbols.Symbols

/** Class values inside annotations of annotations, as `keep-classes` rules follow them, including annotations that annotate each other. */
class MetaAnnotationsSuite extends munit.FunSuite:

  private val lib = Map(
    "lib/Imports.java" -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) public @interface Imports { Class<?> value(); }",
    "lib/X.java"       -> "package lib; public class X { }",
    // `A` names X itself and is annotated with `B`; `B` is annotated with `A`: a cycle.
    "lib/A.java"     -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) @B @Imports(X.class) public @interface A { }",
    "lib/B.java"     -> "package lib; import java.lang.annotation.*; @Retention(RetentionPolicy.RUNTIME) @A public @interface B { }",
    "lib/UsesA.java" -> "package lib; @A public class UsesA { }",
    "lib/UsesB.java" -> "package lib; @B public class UsesB { }",
    "lib/Plain.java" -> "package lib; public class Plain { }"
  )

  private def followedBy(rules: KeepRules, index: ClassIndex, holder: String): Set[String] =
    val info  = index.get(index.syms.classId(holder)).nn
    val found = Set.newBuilder[String]
    rules.metaReferencedClasses(info, index.get)(c => found += index.syms.className(c))
    found.result()

  private def indexOfLibrary(): (ClassIndex, KeepRules) =
    val syms  = new Symbols
    val index = ClassIndex(Vector(source(compile(lib)), ClassSource.jdk()), syms)
    (index, KeepRules.from(syms, Seq(Keep.ClassValues(Keep.classNames("lib/Imports")))))

  test("a class value on an annotation of annotations is followed") {
    val (index, rules) = indexOfLibrary()
    assertEquals(followedBy(rules, index, "lib/UsesA"), Set("lib/X"))
  }

  test("annotations that annotate each other: the second class gets the value too, whichever is asked first") {
    val (index, rules) = indexOfLibrary()
    assertEquals(followedBy(rules, index, "lib/UsesA"), Set("lib/X"))
    // `B` was read while `A` was being read; its answer must not have been remembered without `A`'s value.
    assertEquals(followedBy(rules, index, "lib/UsesB"), Set("lib/X"))
  }

  test("a class with no annotations follows nothing") {
    val (index, rules) = indexOfLibrary()
    assertEquals(followedBy(rules, index, "lib/Plain"), Set.empty[String])
  }
