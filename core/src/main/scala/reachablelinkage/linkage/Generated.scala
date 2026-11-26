package reachablelinkage.linkage

import reachablelinkage.symbols.Collections._
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.{ClassId, Symbols}

/** The classes a framework finds by adding a suffix to the name of a class it was given (jboss-logging loads `Foo_$logger` for `Foo`, as Dagger, MapStruct and
  * other annotation processors do for their output): no instruction names them. Built once per check from the classes that end with a suffix, so asking about a
  * class costs one lookup.
  */
private[linkage] final class Generated private (companionsByBase: Map[String, Vector[ClassId]], syms: Symbols) {

  /** The generated companions of `c`: the classes named after it plus one of the suffixes. */
  def companionsOf(c: ClassId): Vector[ClassId] =
    if (companionsByBase.isEmpty) Vector.empty else companionsByBase.getOrElse(syms.className(c), Vector.empty)
}

private[linkage] object Generated {

  /** The companions the rules' suffixes name, among the classes of `index`. */
  def of(index: ClassIndex, rules: KeepRules): Generated =
    new Generated(companionsByBase(index, rules.generatedSuffixes), index.syms)

  private def companionsByBase(index: ClassIndex, suffixes: Set[String]): Map[String, Vector[ClassId]] =
    suffixes.toVector.flatMap(suffix => companionsWithSuffix(index, suffix)).groupMapOf(_._1)(_._2)

  /** Each class ending with `suffix`, with the name it is a companion of. */
  private def companionsWithSuffix(index: ClassIndex, suffix: String): Vector[(String, ClassId)] =
    index.endingWith(suffix).flatMap(generated => baseName(index.syms.className(generated), suffix).map(base => (base, generated)))

  /** The name before the suffix, unless nothing is left of it. */
  private def baseName(name: String, suffix: String): Option[String] =
    Some(name.dropRight(suffix.length)).filter(_.nonEmpty)
}
