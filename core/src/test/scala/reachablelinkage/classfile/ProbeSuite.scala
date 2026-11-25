package reachablelinkage.classfile

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

/** The stored form of a probe is the only place that knows the prefixes: every kind survives a round trip and none is mistaken for another. */
class ProbeSuite extends munit.FunSuite:

  private inline def caseCount[T](using m: Mirror.SumOf[T]): Int = constValueTuple[m.MirroredElemLabels].size

  private val samples: List[Probe] =
    List(Probe.ClassName("a/b/C"), Probe.NameTail("/kqueue/KQueue"), Probe.MethodName("run"), Probe.LambdaInterface("scala/runtime/java8/JFunction1"))

  test("every kind of probe is sampled, and each survives encoding and decoding") {
    assertEquals(samples.map(_.getClass).distinct.size, caseCount[Probe], "a sample per kind")
    samples.foreach(probe => assertEquals(Probe.decode(Probe.encode(probe)), probe))
  }

  test("a class name is stored as it is, so a summary written before probes were typed still reads") {
    assertEquals(Probe.encode(Probe.ClassName("lib/Missing")), "lib/Missing")
    assertEquals(Probe.decode("lib/Missing"), Probe.ClassName("lib/Missing"))
  }

  test("the kinds are told apart by their stored form alone") {
    val stored = samples.map(Probe.encode)
    assertEquals(stored.distinct.size, stored.size)
    assertEquals(samples.map(p => Probe.decode(Probe.encode(p)).getClass), samples.map(_.getClass))
  }
