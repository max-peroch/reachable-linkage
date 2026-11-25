package reachablelinkage.classpath

import java.nio.charset.StandardCharsets

import reachablelinkage.symbols.ClassId

/** A class listed in a service file of classpath entry `entry` that is not on the classpath. */
private[reachablelinkage] final case class MissingProvider(service: ClassId, provider: ClassId, entry: Int)

/**
 * The `ServiceLoader` providers declared by the classpath: for each `META-INF/services/<service>`
 * file, the classes it lists. A provider is never named by an instruction, so reachability has to
 * start from it explicitly. `byService` and `interfaces` hold the providers that exist on the
 * classpath; `missing` holds the listed ones that do not.
 */
private[reachablelinkage] final class Services private (
    byService: java.util.HashMap[Int, Array[ClassId]],
    val interfaces: Array[ClassId],
    val missing: Vector[MissingProvider]
):

  /** The providers declared for `service`, in classpath order; empty if there are none. */
  def providersOf(service: ClassId): Array[ClassId] =
    val found = byService.get(service.value)
    if found == null then Services.NoProviders else found

private[reachablelinkage] object Services:
  private val NoProviders: Array[ClassId] = Array.empty

  def read(index: ClassIndex): Services =
    val syms     = index.syms
    val provider = new java.util.LinkedHashMap[Int, java.util.LinkedHashSet[Int]]()
    val missing  = scala.collection.mutable.LinkedHashSet.empty[MissingProvider]
    index.sources.iterator.zipWithIndex.foreach: (source, entry) =>
      source.serviceFiles.foreach: file =>
        val service = syms.classId(file.stripPrefix(ClassSource.ServicesDir).replace('.', '/'))
        source.readResource(file).foreach: bytes =>
          parse(new String(bytes, StandardCharsets.UTF_8)).foreach: name =>
            val impl = syms.classId(name.replace('.', '/'))
            if index.exists(impl) then
              provider.computeIfAbsent(service.value, _ => new java.util.LinkedHashSet[Int]()).add(impl.value)
            else missing += MissingProvider(service, impl, entry)
    val byService = new java.util.HashMap[Int, Array[ClassId]]()
    provider.forEach((service, impls) => byService.put(service, impls.toArray(new Array[Integer](0)).map(i => ClassId(i.intValue))))
    new Services(byService, byService.keySet.toArray(new Array[Integer](0)).map(i => ClassId(i.intValue)), missing.toVector)

  /**
   * The provider class names in one service file, with `ServiceLoader`'s rules: `#` starts a comment
   * that runs to the end of the line, and surrounding whitespace and blank lines are ignored.
   */
  def parse(text: String): Vector[String] =
    text.linesIterator.map(line => line.takeWhile(_ != '#').trim).filter(_.nonEmpty).toVector
