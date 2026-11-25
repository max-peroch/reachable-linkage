package reachablelinkage.api

import scala.jdk.CollectionConverters.*

/**
 * How much of a report to show.
 *
 * @param maxGroups        most groups (one broken target each) listed; the rest are counted
 * @param maxSitesPerGroup most places listed under a group; the rest are counted
 * @param demangle         show Scala's generated names as source names (`Foo$` as `object Foo`)
 */
final case class ReportOptions(maxGroups: Int = 50, maxSitesPerGroup: Int = 3, demangle: Boolean = true)

/**
 * A readable, bounded report of conflicts. Conflicts are grouped by what is broken (the category and the target class
 * and member), because one missing class is usually hit from many places; groups come worst first (the most likely to
 * be hit, then the most places), and each lists a few places and counts the rest. A thousand conflicts read as a
 * screenful. Pure: the same conflicts give the same lines.
 */
object Report:

  private final case class Group(category: ConflictCategory, target: String, member: String, sites: List[LinkageConflict]):
    def worst: ConflictLabel = sites.map(_.label).maxBy(_.severity)

  /** The lines of the report: a summary, then the groups. Empty when there are no conflicts. */
  def render(conflicts: java.util.List[LinkageConflict], options: ReportOptions = ReportOptions()): List[String] =
    val all = conflicts.asScala.toList
    if all.isEmpty then Nil
    else
      val groups = all.groupBy(c => (c.category, c.dependency.targetClass, c.dependency.targetMemberName)).toList
        .map((key, sites) => Group(key._1, key._2, key._3, sites))
        .sortBy(g => (-g.worst.severity, -g.sites.size, g.target, g.member, g.category.ordinal))
      val shown = groups.take(options.maxGroups)
      val rest  = groups.drop(options.maxGroups)
      summary(all, groups.size) :: shown.flatMap(lines(_, options)) ++
        (if rest.isEmpty then Nil else List(s"... and ${rest.size} more groups (${rest.map(_.sites.size).sum} conflicts)"))

  /** `12 conflicts in 5 groups: REACHABLE 2, GUARDED 3, LATENT 7`. */
  private def summary(all: List[LinkageConflict], groups: Int): String =
    val byLabel = all.groupBy(_.label).toList.sortBy((label, _) => -label.severity)
      .map((label, cs) => s"$label ${cs.size}").mkString(", ")
    s"${all.size} ${if all.size == 1 then "conflict" else "conflicts"} in $groups ${if groups == 1 then "group" else "groups"}: $byLabel"

  private def lines(group: Group, options: ReportOptions): List[String] =
    def cls(name: String): String = if options.demangle then ScalaNames.clazz(name) else name
    def method(name: String): String = if options.demangle then ScalaNames.method(name) else name
    val first   = group.sites.head
    val labels  = group.sites.groupBy(_.label).toList.sortBy((l, _) => -l.severity).map((l, cs) => s"$l ${cs.size}")
    val mixed   = if labels.size > 1 then s" (${labels.mkString(", ")})" else ""
    val target  = cls(group.target) + (if group.member.isEmpty then "" else s".${if options.demangle then ScalaNames.method(group.member) else group.member}")
    val inlined = if ScalaNames.isInlineAccessor(group.member) then s" [inlined from ${first.existsIn}]" else ""
    val header  = s"[${group.worst}] ${group.category} $target$inlined: ${first.reason}$mixed, ${group.sites.size} ${if group.sites.size == 1 then "use" else "uses"}"
    // One line per place, the most likely to be hit first; a method that breaks at several lines is one place.
    val places = group.sites.sortBy(c => (-c.label.severity, c.dependency.fromClass, c.dependency.fromMethodName, c.dependency.fromLineNumber))
      .distinctBy(c => (c.dependency.fromClass, c.dependency.fromMethodName))
    val shown = places.take(options.maxSitesPerGroup).map: c =>
      val d    = c.dependency
      val at   = if d.fromMethodName.isEmpty then cls(d.fromClass) else s"${cls(d.fromClass)}#${method(d.fromMethodName)}"
      val line = if d.fromLineNumber > 0 then s":${d.fromLineNumber}" else ""
      s"    $at$line  [${c.usedBy}]"
    val more = places.size - shown.size
    header :: shown ++ (if more > 0 then List(s"    ... and $more more places") else Nil)
