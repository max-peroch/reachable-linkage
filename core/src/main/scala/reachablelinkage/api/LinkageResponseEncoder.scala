package reachablelinkage.api

import java.util.{ArrayList, LinkedHashMap, List as JList, Map as JMap}

import scala.jdk.CollectionConverters.*

/** Writes a result, and what was done with it, as maps, lists and strings. */
private[api] object LinkageResponseEncoder:

  def error(message: String): JMap[String, AnyRef] =
    val response = new LinkedHashMap[String, AnyRef]()
    response.put("error", message)
    response

  def encode(result: LinkageResult): JMap[String, AnyRef] =
    val response = new LinkedHashMap[String, AnyRef]()
    response.put("warnings", new ArrayList[String](result.warnings))
    response.put("filtered", filtered(result))
    response.put("conflicts", conflicts(result.conflicts.asScala.toList))
    response

  private def filtered(result: LinkageResult): JList[JList[String]] =
    val list = new ArrayList[JList[String]]()
    result.filtered.asScala.foreach(effect => list.add(java.util.Arrays.asList(effect.kind.name, effect.removed.toString)))
    list

  private def conflicts(all: List[LinkageConflict]): JList[JMap[String, AnyRef]] =
    val list = new ArrayList[JMap[String, AnyRef]]()
    all.foreach(c => list.add(conflict(c)))
    list

  /** `failing` is what the default fail level (`FailOn.REACHABLE`) fails on; `reportedByMissinglink` is the view of a plugin in compatibility mode. */
  private def conflict(c: LinkageConflict): JMap[String, AnyRef] =
    val map = new LinkedHashMap[String, AnyRef]()
    map.put("category", c.category.name)
    map.put("label", c.label.name)
    map.put("usedBy", c.usedBy)
    map.put("existsIn", c.existsIn)
    map.put("reason", c.reason)
    map.put("describe", c.dependency.describe())
    map.put("fromClass", c.dependency.fromClass)
    map.put("fromMethodName", c.dependency.fromMethodName)
    map.put("fromMethodDescriptor", c.dependency.fromMethodDescriptor)
    map.put("fromLineNumber", Int.box(c.dependency.fromLineNumber))
    map.put("targetClass", c.dependency.targetClass)
    map.put("targetMemberName", c.dependency.targetMemberName)
    map.put("targetMemberDescriptor", c.dependency.targetMemberDescriptor)
    map.put("failing", Boolean.box(ConflictLabel.counts(c.label)))
    map.put("reportedByMissinglink", Boolean.box(c.category.reportedByMissinglink))
    map
