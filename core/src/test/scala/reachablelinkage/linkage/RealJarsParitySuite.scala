package reachablelinkage.linkage

import reachablelinkage.fixture.Scopes
import java.nio.file.Paths
import reachablelinkage.classpath.{ClassIndex, ClassSource}
import reachablelinkage.fixture.MissinglinkOracle
import reachablelinkage.fixture.MissinglinkOracle.Key
import reachablelinkage.symbols.{ClassId, Symbols}

/**
 * Parity on real code: both tools scan whole resolved classpaths the way sbt-missinglink does with
 * `missinglinkScanDependencies := true` (every class is a root, every entry is checked).
 *
 * Differences must fall into explained categories, each checked against the real class data:
 *  - we also report a missing ancestor (superclass or interface, direct or not) of the class itself
 *    or of a class it calls into. missinglink only notices via a constructor call, or says "method
 *    missing" through the hole where we name the class;
 *  - we report a concrete class that leaves an inherited abstract method unimplemented (missinglink
 *    does not look); on these classpaths that is only Scala 3's `$outer` accessors of inner traits;
 *  - we report a class that is only used by a type instruction (`checkcast`, `instanceof`, `new`).
 *    missinglink only reports missing classes at method calls and field accesses, but a cast to a
 *    missing class fails at run time with `NoClassDefFoundError`.
 */
class RealJarsParitySuite extends munit.FunSuite:
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  private val props =
    val p  = new java.util.Properties()
    val in = getClass.getResourceAsStream("/scala-compilers.properties")
    try p.load(in) finally in.close()
    p

  for key <- Seq("2.12", "2.13", "3-lts", "3-next") do
    test(s"whole classpath of the Scala $key compiler: same conflicts as missinglink-core") {
      val srcs  = props.getProperty(key).split(java.io.File.pathSeparator).toSeq.map(j => ClassSource.open(Paths.get(j)).toOption.get)
      val theirs = MissinglinkOracle.checkAll(srcs)

      val syms  = new Symbols
      val index = ClassIndex((srcs :+ ClassSource.jdk()).toIndexedSeq, syms)
      val all   = srcs.indices.toVector
      val ours  = Checker.check(index, Scopes.of(index, all, all, all)).flatMap(MissinglinkOracle.normalise(_, syms)).toSet

      def ancestors(from: ClassId): Set[ClassId] =
        val seen  = scala.collection.mutable.Set.empty[ClassId]
        var todo  = List(from)
        while todo.nonEmpty do
          val c = todo.head; todo = todo.tail
          index.get(c) match
            case null => ()
            case info =>
              (info.superName +: info.interfaces.toSeq).filterNot(_.isNone).foreach: p =>
                if seen.add(p) then todo = p :: todo
        seen.toSet
      // Scala 3's abstract `$outer` accessors of inner traits that implementers leave out: real, harmless.
      def outerAccessor(k: Key): Boolean =
        k.category == "ABSTRACT_METHOD_NOT_IMPLEMENTED" && k.member.endsWith("$$$outer")
      def explainedExtra(k: Key): Boolean =
        outerAccessor(k) || k.category == "CLASS_NOT_FOUND" && {
          val from   = syms.findClass(k.from)
          val target = syms.findClass(k.target)
          val info   = index.get(from)
          def owners(refs: Array[Int], stride: Int): Iterator[ClassId] = refs.iterator.grouped(stride).map(g => ClassId(g.head))
          info != null && (
            ancestors(from).contains(target) ||
            info.classRefs.contains(target.value) ||
            (owners(info.methodRefs, 4) ++ owners(info.fieldRefs, 3)).exists(o => ancestors(o).contains(target))
          )
        }
      // missinglink: "method X.m missing" because X has a missing ancestor. Ours: "class A missing".
      def viaHole(k: Key): Boolean =
        k.category == "METHOD_SIGNATURE_NOT_FOUND" && {
          val owner = syms.findClass(k.target)
          val lineage = ancestors(owner) + owner
          ours.exists(o => o.category == "CLASS_NOT_FOUND" && o.from == k.from && lineage.contains(syms.findClass(o.target)))
        }

      val missed = (theirs -- ours).filterNot(viaHole)
      val added  = (ours -- theirs).filterNot(explainedExtra)
      assertEquals(missed, Set.empty[Key], "conflicts missinglink reports that we miss")
      assertEquals(added, Set.empty[Key], "conflicts we report beyond ancestor and type-instruction breaks")
      println(s"[parity] scala $key: ${srcs.size} jars, missinglink=${theirs.size} ours=${ours.size}")
    }
