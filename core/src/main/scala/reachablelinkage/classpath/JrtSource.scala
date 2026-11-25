package reachablelinkage.classpath

import java.lang.module.ModuleFinder
import java.lang.module.ModuleReader
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/**
 * The running JDK's classes. The listing comes from [[JdkListing]] (from disk when a cache directory
 * is given and holds a valid one); class bytes are read lazily through the module system, one
 * `ModuleReader` per module, opened when first needed.
 */
private[classpath] final class JrtSource(cacheDirectory: Option[Path]) extends ClassSource:
  private val listing = JdkListing.load(cacheDirectory)
  private val moduleOfClass = new java.util.HashMap[String, Integer](listing.names.length * 2)
  listing.names.indices.foreach(i => moduleOfClass.put(listing.names(i), Integer.valueOf(listing.moduleOf(i))))

  private val finder  = ModuleFinder.ofSystem()
  private val readers = new Array[ModuleReader | Null](listing.modules.length)

  val origin: Path = Paths.get(System.getProperty("java.home"))

  // The packages each module exports to everyone (not `exports p to m`), read from its descriptor on first use.
  private val exportedPackages = new Array[Option[Set[String]] | Null](listing.modules.length)

  private def exportedBy(module: Int): Option[Set[String]] =
    exportedPackages(module) match
      case null =>
        val packages = finder.find(listing.modules(module)).toScala.map(_.descriptor.exports.asScala.filterNot(_.isQualified).map(_.source).toSet)
        exportedPackages(module) = packages
        packages
      case known => known

  override def isExported(internalName: String): Boolean =
    moduleOfClass.get(internalName) match
      case null => true
      case m =>
        val dot = internalName.lastIndexOf('/')
        // A module the JDK no longer has, or a class in the unnamed package, tells us nothing: not a reason to report.
        dot < 0 || exportedBy(m.intValue).forall(_.contains(internalName.substring(0, dot).replace('/', '.')))

  def classNames: Iterator[String] = listing.names.iterator

  override def problems: List[String] =
    listing.unreadable.map(module => s"the JDK module $module could not be read, so its classes are treated as missing")

  def read(internalName: String): Option[Array[Byte]] =
    moduleOfClass.get(internalName) match
      case null => None
      case m =>
        // A module the listing names but the JDK no longer has makes its classes unreadable, not an error.
        readerFor(m.intValue).flatMap: reader =>
          reader.read(internalName + ".class").toScala.map: buffer =>
            val bytes = new Array[Byte](buffer.remaining)
            buffer.get(bytes)
            reader.release(buffer)
            bytes

  private def readerFor(module: Int): Option[ModuleReader] =
    readers(module) match
      case null =>
        // A module whose reader cannot be opened makes its classes unreadable, not an error.
        try
          finder.find(listing.modules(module)).toScala.map: ref =>
            val opened = ref.open()
            readers(module) = opened
            opened
        catch case _: java.io.IOException => None
      case existing => Some(existing)

  def close(): Unit =
    readers.indices.foreach: i =>
      if readers(i) != null then
        readers(i).nn.close()
        readers(i) = null
