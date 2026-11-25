package reachablelinkage.api

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import reachablelinkage.api.KeepRuleError.Problem
import reachablelinkage.linkage.Keep

/**
 * A rule saying what a framework calls by reflection, so code a check would call unreachable is not.
 * `names` are class names such as `javax.inject.Inject` (the annotations, for every kind but `KEEP_OVERRIDES`, where they are the
 * supertypes); the kinds that name calls take `owner.method`, `KEEP_CLASSES_NAMED_IN` takes file names and `KEEP_GENERATED_CLASSES` takes suffixes.
 * Matching is by annotation or supertype only, so a rule over-approximates. Built by [[KeepRule.of]] (which checks the names), so it is never empty
 * and every name is well formed. The built-in packs are [[KeepRule.standard]]; a request that lists
 * its own rules replaces them, so add `standard()` to keep them.
 *
 * In a file, one rule per line, `#` starts a comment:
 * {{{
 * keep-members            javax.inject.Inject com.google.inject.Provides
 * keep-no-arg-constructor com.fasterxml.jackson.annotation.JsonProperty
 * follow-class-values     com.google.inject.ImplementedBy
 * keep-overrides          javax.servlet.http.HttpServlet javax.servlet.Filter
 * keep-bean-arguments     com.fasterxml.jackson.databind.ObjectMapper.readValue
 * keep-built-arguments    com.google.gson.Gson.fromJson
 * keep-classes-named-in   reference.conf META-INF/persistence.xml
 * keep-constructed-arguments org.apache.pekko.actor.Props.create
 * keep-constructors       org.springframework.stereotype.Component
 * requires-classes        org.springframework.boot.autoconfigure.condition.ConditionalOnClass
 * keep-generated-classes  _$logger _$bundle
 * }}}
 */
final case class KeepRule private (kind: KeepRuleKind, names: java.util.List[String])

object KeepRule:
  private val Directives: Map[String, KeepRuleKind] = RuleKinds.directives

  /** A rule over `names` (dotted class names), or what is wrong with them. */
  def of(kind: KeepRuleKind, names: List[String]): Either[Problem, KeepRule] =
    if names.isEmpty then Left(Problem.NoNames)
    else names.find(n => !RuleKinds.describe(kind).form.isValid(n)).map(Problem.InvalidName(_))
      .toLeft(new KeepRule(kind, names.distinct.asJava))

  /** The built-in packs: what `Linkage.check` uses unless the request lists other rules. */
  def standard(): java.util.List[KeepRule] = standardRules.asJava

  private val standardRules: List[KeepRule] = Keep.standard.flatMap(ruleOf)

  /** The facade's rule for one of the engine's. */
  private def ruleOf(keep: Keep): Option[KeepRule] =
    RuleKinds.kindOf(keep).map((kind, names) => new KeepRule(kind, RuleKinds.describe(kind).form.toFacade(names).asJava))

  /** The rules a check runs, as the engine's. */
  private[api] def toKeep(rules: java.util.List[KeepRule]): List[Keep] =
    rules.asScala.toList.map(keepOf)

  private def keepOf(rule: KeepRule): Keep =
    val description = RuleKinds.describe(rule.kind)
    description.build(description.form.toEngine(rule.names.asScala.toList))

  /** Reads a rules file, saying precisely what is wrong with it if it cannot. */
  def parse(file: Path): Either[KeepRuleError, List[KeepRule]] =
    for
      lines <- ReadLines.of(file).left.map(f => KeepRuleError.Unreadable(f.file, f.cause))
      rules <- lines.zipWithIndex.foldLeft[Either[KeepRuleError, List[KeepRule]]](Right(Nil)): (acc, numbered) =>
                 val (text, i) = numbered
                 for
                   done <- acc
                   rule <- parseLine(text).left.map(KeepRuleError.BadLine(file, i + 1, _))
                 yield rule.fold(done)(_ :: done)
    yield rules.reverse

  /** The rule on a line; none for a blank or comment line. */
  private def parseLine(text: String): Either[Problem, Option[KeepRule]] =
    text.takeWhile(_ != '#').trim.split("\\s+").toList.filter(_.nonEmpty) match
      case Nil                  => Right(None)
      case directive :: names   =>
        Directives.get(directive).toRight(Problem.UnknownDirective(directive)).flatMap(of(_, names)).map(Some(_))
